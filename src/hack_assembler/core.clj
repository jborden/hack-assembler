(ns hack-assembler.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :refer [defparser]]))

(declare hack-asm-parser)
(defparser hack-asm-parser
  (slurp "resources/bnf/hack.bnf"))

(def dest->dbits
  {nil "000"
   "M" "001"
   "D" "010"
   "MD" "011"
   "A" "100"
   "AM" "101"
   "AD" "110"
   "AMD" "111"})

(def jump->jbits
  {nil   "000"
   "JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(def comp->cbits
  {"0"   "101010"
   "1"   "111111"
   "-1"  "111010"
   "D"   "001100"
   "A"   "110000" "M"   "110000"
   "!D"  "001101"
   "!A"  "110001" "!M"  "110001"
   "-D"  "001111"
   "-A"  "110011" "-M"  "110011"
   "D+1" "011111"
   "A+1" "110111" "M+1" "110111"
   "D-1" "001110"
   "A-1" "110010" "M-1" "110010"
   "D+A" "000010" "D+M" "000010"
   "D-A" "010011" "D-M" "010011"
   "A-D" "000111" "M-D" "000111"
   "D&A" "000000" "D&M" "000000"
   "D|A" "010101" "D|M" "010101"})

(def comp->abit
  {"0"   "0"
   "1"   "0"
   "-1"  "0"
   "D"   "0"
   "A"   "0" "M"   "1"
   "!D"  "0"
   "!A"  "0" "!M"  "1"
   "-D"  "0"
   "-A"  "0" "-M"  "1"
   "D+1" "0"
   "A+1" "0" "M+1" "1"
   "D-1" "0"
   "A-1" "0" "M-1" "1"
   "D+A" "0" "D+M" "1"
   "D-A" "0" "D-M" "1"
   "A-D" "0" "M-D" "1"
   "D&A" "0" "D&M" "1"
   "D|A" "0" "D|M" "1"})

;; @i starts at 15
(def predefined-symbols
  (merge
   (apply merge (map #(hash-map (str "R" %) (str %)) (range 0 16)))
   {"SCREEN" "16384"
    "KBD" "24576"
    "SP" "0"
    "LCL" "1"
    "ARG" "2"
    "THIS" "3"
    "THAT" "4"}))

;; second pass - add var symbols

;; first pass - add label symbols
(defn first-pass
  [tokens]
  (->> tokens
       (map-indexed (fn [idx itm]
                      (when (= (first itm) :LABEL_SYMBOL)
                        [(-> itm second second) idx])))
       (into [])
       (filterv (comp not nil?))
       ;; increment it by its position
       (map-indexed (fn [idx itm]
                      [(first itm) (str (- (second itm) idx))]))
       (mapv (partial apply hash-map))
       (apply merge)))

;; https://stackoverflow.com/questions/21448788/printing-the-binary-value-of-a-number-in-clojure
(defn print-bits
  "Convert integer to string representation in binary, giving two's complement negative numbers"
  [b]
  (let [class-name (.getName (class b))
        is-byte (= "java.lang.Byte" class-name)
        num-bits (clojure.lang.Reflector/getStaticField class-name "SIZE")
        bin-str-fn #(clojure.lang.Reflector/invokeStaticMethod
                     (if is-byte "java.lang.Integer" class-name)
                     "toBinaryString"
                     (to-array [%]))
        bit-string (if is-byte
                     (string/join (take-last 8 (bin-str-fn (Byte/toUnsignedInt b))))
                     (bin-str-fn b))]
    (str (string/join (repeat (- num-bits (count bit-string)) \0))
         bit-string)))

(defn int-string->16-bit-string
  "Convert a string int value into a binary string representation"
  [s]
  (->> s read-string print-bits reverse (take 15) reverse (apply str) (str "0")))

(defn a-instruction->binary-string
  "Given a vector representing an a-instruction, convert it to a binary string"
  [v env]
  (let [value (->> v rest (into (sorted-map)) :VALUE)]
    (if (re-matches #"\d+" value)
      (int-string->16-bit-string value)
      (int-string->16-bit-string (get env value)))))

(defn c-instruction->binary-string
  "Given a vector representing a c-instruction, convert it to a binary string"
  [v]
  (let [{:keys [COMP DEST JUMP]} (->> v rest (into (sorted-map)))]
    (str "111"
         (get comp->abit COMP)
         (get comp->cbits COMP)
         (get dest->dbits DEST)
         (get jump->jbits JUMP))))

(defn tokens->binary-string
  [tokens env]
  "Given a set of tokens, convert them to their binary string representation for the hack computer"
  (map #(let [instruction-type (first %)]
          (condp = instruction-type
            :A_INSTRUCTION (a-instruction->binary-string % env)
            :C_INSTRUCTION (c-instruction->binary-string %)))
       (filterv #(not= (first %) :LABEL_SYMBOL) tokens)))

(defn tokenize
  "Given a string, tokenize it"
  [s]
  (hack-asm-parser s))

(defn assemble-file
  "Given a filename, output a .hack output file representing it"
  [filename]
  (let [io-file (io/file filename)
        filepath (.getPath io-file)
        output-filename (string/replace filepath #".asm" ".hack")
        input (slurp filepath)
        tokens (->> input tokenize rest (into []))
        labels-symbols (first-pass tokens)
        env (merge labels-symbols predefined-symbols)]
    (spit output-filename
          (-> input tokenize
              rest
              (into [])
              (tokens->binary-string env)
              (->> (string/join "\n"))
              (str "\n")))))

(ns clojure-assembler.core
  (:use [clojure.pprint :only [cl-format]]))

(def comps {"0"   "0101010"
            "1"   "0111111"
            "-1"  "0111010"
            "D"   "0001100"
            "A"   "0110000"
            "M"   "1110000"
            "!D"  "0001101"
            "!A"  "0110001"
            "!M"  "1110001"
            "-D"  "0001111"
            "-A"  "0110011"
            "-M"  "1110011"
            "D+1" "0011111"
            "A+1" "0110111"
            "M+1" "1110111"
            "D-1" "0001110"
            "A-1" "0110010"
            "M-1" "1110010"
            "D+A" "0000010"
            "D+M" "1000010"
            "D-A" "0010011"
            "D-M" "1010011"
            "A-D" "0000111"
            "M-D" "1000111"
            "D&A" "0000000"
            "D&M" "1000000"
            "D|A" "0010101"
            "D|M" "1010101"})

(def dests {"M"   "001"
            "D"   "010"
            "MD"  "011"
            "A"   "100"
            "AM"  "101"
            "AD"  "110"
            "ADM" "111"})

(def jumps {"JGT" "001"
            "JEQ" "010"
            "JGE" "011"
            "JLT" "100"
            "JNE" "101"
            "JLE" "110"
            "JMP" "111"})

(def predefined-symbols {"SP"     0
                         "LCL"    1
                         "ARG"    2
                         "THIS"   3
                         "THAT"   4
                         "R0"     0
                         "R1"     1
                         "R2"     2
                         "R3"     3
                         "R4"     4
                         "R5"     5
                         "R6"     6
                         "R7"     7
                         "R8"     8
                         "R9"     9
                         "R10"    10
                         "R11"    11
                         "R12"    12
                         "R13"    13
                         "R14"    14
                         "R15"    15
                         "SCREEN" 16384
                         "KBD"    24576})

(defn re-escape [string]
  (clojure.string/replace string #"[+|]" "\\\\$0"))

(def a-command-re #"^@(?:([0-9]+)|([A-Za-z_][A-Za-z0-9_]*))")
(def c-command-re (re-pattern (str "^([ADM]=)?"
                                   "(" (clojure.string/join "|" (map re-escape (keys comps))) ")"
                                   "(?:;("
                                   (clojure.string/join "|" (keys jumps))
                                   "))?$")))

(defn strip-comments [line]
  (clojure.string/replace-first line #"//.*$" ""))

(defn remove-whitespace [line]
  (clojure.string/replace line #"\s+" ""))

(defn normalize [line]
  (remove-whitespace (strip-comments line)))

(defn parse [asm symbols addr]
  (let [line (normalize asm)
        [_ a1 a2] (re-matches a-command-re line)
        [_ dst cmp jmp] (re-matches c-command-re line)]
    (cond
      (empty? line)
      [nil symbols addr]
      (or a1 a2)
      [[:a (Integer/parseInt a1)] symbols addr]
      cmp
      [[:c cmp (str (first dst)) jmp] symbols addr])))

(defn parse-lines
  ([lines]
   (parse-lines lines [] predefined-symbols 0x10))
  ([lines commands symbols addr]
   (if (empty? lines)
     [commands symbols]
     (let [[command new-symbols new-addr] (parse (first lines) symbols addr)]
       (parse-lines (rest lines)
                    (conj commands command)
                    new-symbols
                    new-addr)))))

(defmulti translate first)
(defmethod translate :a [[_ address]]
  (str "0" (cl-format nil "~15,'0b" address)))
(defmethod translate :c [[_ comp dest jump]]
  (str "111" (comps comp) (dests dest "000") (jumps jump "000")))
(defmethod translate nil [_]
  nil)

(defn -main []
  (let [asm (line-seq (java.io.BufferedReader. *in*))
        [commands _] (parse-lines asm)]
    (doseq [command commands]
      (if-let [hack (translate command)]
        (println hack)))))

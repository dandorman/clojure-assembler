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

(def a-command-re #"^@(?:([0-9]+)|([A-Za-z_:$.][A-Za-z0-9_:$.]*))")

(def c-command-re (re-pattern (str "^(?:(" (clojure.string/join "|" (keys dests)) ")=)?"
                                   "(" (clojure.string/join "|" (map re-escape (keys comps))) ")"
                                   "(?:;("
                                   (clojure.string/join "|" (keys jumps))
                                   "))?$")))

(def l-command-re #"\(([A-Za-z$.:_][A-Za-z0-9$.:_]*)\)")

(defn strip-comments [line]
  (clojure.string/replace-first line #"//.*$" ""))

(defn remove-whitespace [line]
  (clojure.string/replace line #"\s+" ""))

(defn normalize [line]
  (remove-whitespace (strip-comments line)))

(defn parse [asm commands line-number symbols addr]
  (let [line (normalize asm)
        [_ addr-literal addr-label] (re-matches a-command-re line)
        [_ dst cmp jmp] (re-matches c-command-re line)
        [_ loop-label] (re-matches l-command-re line)]
    (cond
      (empty? line)
      [commands
       line-number
       symbols
       addr]

      addr-literal
      [(conj commands [:a (Integer/parseInt addr-literal)])
       (inc line-number)
       symbols
       addr]

      addr-label
      (if (get predefined-symbols addr-label)
        [(conj commands [:a addr-label])
         (inc line-number)
         symbols
         addr]
        (let [new-symbols (assoc symbols addr-label (get symbols addr-label addr))
              new-addr (if (= addr (get new-symbols addr-label)) (inc addr) addr)]
          [(conj commands [:a addr-label])
           (inc line-number)
           new-symbols
           new-addr]))

      cmp
      [(conj commands [:c cmp dst jmp])
       (inc line-number)
       symbols
       addr]

      loop-label
      (let [new-symbols (assoc symbols loop-label line-number)]
        [commands
         line-number
         new-symbols
         addr]))))

(defn parse-lines
  ([lines]
   (parse-lines lines [] 0 predefined-symbols 0x10))
  ([initial-lines initial-commands initial-line-number initial-symbols initial-addr]
   (loop [lines       initial-lines
          commands    initial-commands
          line-number initial-line-number
          symbols     initial-symbols
          addr        initial-addr]
     (if (empty? lines)
       [commands symbols]
       (let [[new-commands new-line-number new-symbols new-addr] (parse (first lines) commands line-number symbols addr)]
         (recur (rest lines)
                new-commands
                new-line-number
                new-symbols
                new-addr))))))

(defmulti translate (fn [[command _] _] command))
(defmethod translate :a [[_ address] symbols]
  (str "0" (cl-format nil "~15,'0b" (get symbols address address))))
(defmethod translate :c [[_ cmp dest jump] _]
  (str "111" (comps cmp) (dests dest "000") (jumps jump "000")))

(defn -main []
  (let [asm (line-seq (java.io.BufferedReader. *in*))
        [commands symbols] (parse-lines asm)]
    (doseq [command commands]
      (if-let [hack (translate command symbols)]
        (println hack)))))

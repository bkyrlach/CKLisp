(def true (.TRUE java.lang.Boolean))
(def false (.FALSE java.lang.Boolean))
(def cons (fn (item lst) (.$colon$colon lst item)))

(def defmacro (macro (sym & body)
  (let (m (cons 'macro body))
    '(def ~sym ~m))))

(defmacro defn (sym & body) 
  (let (f (cons 'fn body)) 
  	'(def ~sym ~f)))

(defn eval (exp) 
  (let (env *curenv*
        reader *reader*
        pexp (.parseAll reader (.exp reader) exp))
    (.handle org.cklisp.Handler (.get pexp) env)))

;Doesn't work. :(
(defn exit () (.exit java.lang.System 0))

(defn first (lst) (.head lst))
(defn rest (lst) (.tail lst))

(def sysin (java.io.BufferedReader. (java.io.InputStreamReader. (.in java.lang.System))))

(defn println (x) (.println (.out java.lang.System) (.toString x)))
(defn print (x) (.print (.out java.lang.System) (.toString x)))
(defn read () (.read sysin))
(defn readln () (.readLine sysin))

(defn =  (x y) (.equals x y))
(defn +  (x y) (.plus org.cklisp.Math x y))
(defn -  (x y) (.minus org.cklisp.Math x y))
(defn *  (x y) (.times org.cklisp.Math x y))
(defn >  (x y) (.greater org.cklisp.Math x y))
(defn <  (x y) (.less org.cklisp.Math x y))
(defn >= (x y) (.greaterEq org.cklisp.Math x y))
(defn <= (x y) (.lessEq org.cklisp.Math x y))
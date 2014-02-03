(ns hack-clj.regex)

(def c-arithmetic #"^add|^sub|^neg|^eq|^gt|^lt|^and|^or|^not")

(def c-push #"^push")

(def c-pop #"^pop")

(def c-label #"^label")

(def c-if #"^if")

(def c-function #"^function")

(def c-return #"^return$")

(def c-call #"^call")

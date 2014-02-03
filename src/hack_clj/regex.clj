(ns hack-clj.regex)

(def c-add #"^add")

(def c-sub #"^sub")

(def c-neg #"^neg")

(def c-eq #"^eq")

(def c-gt #"^gt")

(def c-lt #"^lt")

(def c-and #"^and")

(def c-or #"^or")

(def c-not #"^not")

(def c-arithmetic (clojure.string/join "|" [c-add c-sub c-neg c-eq c-gt c-lt c-and c-or c-not]))
;(def c-arithmetic #"^add|^sub|^neg|^eq|^gt|^lt|^and|^or|^not")

(def c-push #"^push")

(def c-pop #"^pop")

(def c-label #"^label")

(def c-if #"^if")

(def c-function #"^function")

(def c-return #"^return$")

(def c-call #"^call")

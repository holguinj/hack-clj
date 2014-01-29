(ns hack-clj.core)

(defn a-instruction? [asm]
  (false? (nil? (re-find #"^\@" asm))))


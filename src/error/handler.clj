(ns error.handler
  (:import [javax.naming NamingException]))

(def *handlers* [])

(defn- e-to-map [e]
  {:msg (.getMessage e) :type (class e) :exception e})

(defn- wrapped? [e]
  (and (instance? NamingException e) (map? (.getResolvedObj e))))

(defn unwrap [e]
  (if (wrapped? e)
    (let [r (.getResolvedObj e)]
      (if (map? r) r
          (throw (IllegalStateException.
                  "Wrapped object is not a map - must be a real NamingException?"))))
    (e-to-map e)))

(defprotocol Raisable
  (raise [this])
  (wrap [this]))

(extend-protocol Raisable
  clojure.lang.IPersistentMap
  (raise [this] (throw (wrap this)))
  (wrap [this] (let [e (NamingException. (or (:msg this) ""))]
               (.setResolvedObj e this)
               e))

  java.lang.Throwable
  (raise [this] (raise (e-to-map this)))
  (wrap [this] (wrap (e-to-map this))))

(defn rewrap [e addmap]
  (let [m (unwrap e)
        m (merge addmap m)]
    (if (wrapped? e) (do (.setResolvedObj e m)
                         e)
        (wrap m))))

(defn recover [err recovery]
  (let [recoveryfn (recovery err)]
    (cond (nil? recoveryfn) (throw (IllegalStateException.
                                    (str "Recovery chosen that does not exist: " recovery)))
          (fn? recoveryfn) (recoveryfn err)
          :else (throw (IllegalArgumentException.
                        (format "Recovery %s needs to be a function with one argument, instead got: %s"
                                recovery recoveryfn))))))

(defmacro with-handlers "Runs code in an error handling environment.

  Executes body, if an error is raised, pass it to each of the
handlers in hlist.  Each handler should be a function that should take
an error map as an argument, and returns one of the following:

  1) A value which will be returned as the value of the whole form

  2) The original error, if the handler doesn't handle this kind of
  error.

The error map will have whatever keys it was created with,
typically :msg will be the text of the error, and :type will be the
type.

Within the handler, you can also choose a pre-defined recovery by
retrieving it from the error map, and calling it.  The recover
function does this for you.  In most cases, the call to recover will
be the entire body of the handler."
  [dispatch-fn hlist & body]
  `(binding [*handlers* (concat ~hlist *handlers*) ] ;chain handlers together
     (try ~@body
          (catch Throwable ne#
            (let [unwrapped# (unwrap ne#)
                  selected# (~dispatch-fn unwrapped#)
                  chosen-handler# (first (filter #(isa? selected# (~dispatch-fn (meta %)))
                                                  *handlers*))
                  unhandled# (or (:exception unwrapped#) ne#)] ;if the original error was an exception, retrieve it to throw if it is not handled.
              (if (nil? chosen-handler#)
                (throw unhandled#) 
                (chosen-handler# unwrapped#)))))))

(defmacro add-recoveries "Executes body and attaches all the key/value
pairs in m to any error that occurs.  An error handler further down
the call stack can examine the data in the map.  Recovery functions
can be created by adding keys whose values are functions.  Recovery
functions should take one argument - the error."
  [m & body]
  `(try ~@body
        (catch Throwable ne#
          (throw (rewrap ne# ~m)))))

(defmacro handle-type [type arglist & body]
  (if (not= (count arglist) 1) (throw (IllegalArgumentException.
                                    (str "Type handlers can only take one argument, got " (count arglist)))))
  (let [errname (first arglist)]
    `(with-meta
       (fn ~arglist (do ~@body))
       {:type ~type})))

(defn expect "Handle an error of a given type with a no-op function."
  [type]
  (handle-type type [_] nil))





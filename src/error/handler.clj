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

(defmacro with-handlers-dispatch "Runs code in an error handling environment.

  Executes body. If an error is raised (or an exception is thrown),
the list of error handling functions is passed through the dispatch
function dispatch-fn to determine which handler will be selected.  The
dispatch function will be called on the error map, and then on the
metadata of each handler.  The return values of those calls will be
compared using isa?, and the first handler to match the error will be
called.  Java exceptions will be converted to error maps with the
keys :msg and :type.

   Each handler should be a function that should take
an error map as an argument.

Within the handler, you can also choose a pre-defined recovery
function by retrieving it from the error map, and calling it.  The
'recover' function does this for you.  In most cases, the call to
recover will be the entire body of the handler."
  [dispatch-fn hlist & body]
  `(do
     (if-not (ifn? ~dispatch-fn)
       (throw (IllegalArgumentException. "First argument to with-handlers must be a function.")))
     (if-not (every? ifn? ~hlist)
       (throw (IllegalArgumentException. "All handlers must be functions.")))
     (binding [*handlers* (concat ~hlist *handlers*) ] ;chain handlers together
       (try ~@body
            (catch Throwable ne#
              (let [unwrapped# (unwrap ne#)
                    selected# (~dispatch-fn unwrapped#)
                    chosen-handler# (first (filter #(isa? selected# (~dispatch-fn (meta %)))
                                                   *handlers*))
                    unhandled# (or (:exception unwrapped#) ne#)] ;if the original error was an exception, retrieve it to throw if it is not handled.
                (if (nil? chosen-handler#)
                  (throw unhandled#) 
                  (chosen-handler# unwrapped#))))))))

(defmacro with-handlers [hlist & body]
  `(with-handlers-dispatch :type ~hlist ~@body))

(defmacro add-recoveries "Executes body and attaches all the key/value
pairs in m to any error that occurs.  An error handler further down
the call stack can examine the data in the map.  Recovery functions
can be created by adding keys whose values are functions.  Recovery
functions should take one argument - the error."
  [m & body]
  `(try ~@body
        (catch Throwable ne#
          (throw (rewrap ne# ~m)))))

(defmacro handle "Creates a handler that can be dispatched by :type." [type arglist & body]
  (if (not= (count arglist) 1) (throw (IllegalArgumentException.
                                    (str "Type handlers can only take one argument, got " (count arglist)))))
  `(with-meta
     (fn ~arglist ~@body)
     {:type ~type}))

(defn ignore "Handle an error of a given type with a no-op function."
  [type]
  (handle type [_] nil))





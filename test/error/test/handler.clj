(ns test.error.handler
  [:use clojure.test error.handler])

;; a low level fn that can cause errors
(defn error-prone [n]
  (cond
   (> n 300) (raise {:type :OtherError :msg "That's just ridiculous."})
   (> n 200) (raise (IllegalStateException. "Wayy Too big!"))  ;;java exceptions can participate normally
   (> n 100) (throw (IllegalArgumentException. "Too big!"))  ;;java exceptions can participate normally
   (> n 0) (inc n)
   :else (raise {:msg "Negative number!" :number n :type :NumberError})))  ;;clojure errors are just maps

;;a fn that adds recoveries in a middle layer
(defn do-stuff [n]
  (add-recoveries
   {:return-zero (constantly 0)
    :retry #(error-prone (Math/abs (:number %)))}
   (error-prone n)))

;;define handler fn yourself and call middle layer
(testing "basic handler with manual metadata"
  (is (= 42
         (with-handlers :type [^{:type :NumberError} (constantly 42)]
           (do-stuff -5)))))

(testing "basic handler with recovery call"
  (is (= 6
         (with-handlers :type [^{:type :NumberError} (fn [e] (recover e :retry))]
           (do-stuff -5)))))

;;use macro to specify handlers, show that recoveries can be added at any level
(def test-handlers
  [ (handle-type :NumberError [e] (recover e :return-zero)) ;;choose a predefined recovery
    (handle-type :OtherError [e] 0)
    (handle-type IllegalStateException [e] 201)])

(testing "multiple handlers"
  (is (thrown? IllegalArgumentException (with-handlers :type test-handlers
                                          (do-stuff 105))))
  (is (= 201 (with-handlers :type test-handlers 
               (do-stuff 255))))

  (is (= 0 (with-handlers :type test-handlers 
               (do-stuff 321)))))

(testing "nested handlers"
  (is (= 42 (with-handlers :type [(handle-type :OtherError [_] 42)]
              (with-handlers :type [(handle-type :NumberError [e] (recover e :return-zero))]
                (do-stuff 305))))))

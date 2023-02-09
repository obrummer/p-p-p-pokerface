(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn rank [card]
  (let [[rank-definition _] card]
  (if (Character/isDigit rank-definition) 
    (Integer/valueOf (str rank-definition)) 
    (replacements rank-definition))))

(defn suit [card]
  (let [[_ suit-definition] card]
  (str suit-definition)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        amounts (vals (frequencies ranks))]
       (> (count (filter (fn [x] (== x 2)) amounts)) 0)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        amounts (vals (frequencies ranks))]
       (> (count (filter (fn [x] (== x 3)) amounts)) 0)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        amounts (vals (frequencies ranks))]
       (> (count (filter (fn [x] (== x 4)) amounts)) 0)))

(defn flush? [hand]
  (let [suits (map suit hand)
        amounts (vals (frequencies suits))]
       (> (count (filter (fn [x] (== x 5)) amounts)) 0)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        amounts (sort (vals (frequencies ranks)))]
       (= amounts '(2 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        amounts (sort (vals (frequencies ranks)))]
       (or (= amounts '(1 2 2)) (= amounts '(1 4)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-as-integers (sort ranks)
        minimum (apply min ranks-as-integers)
        maximum (apply max ranks-as-integers)]
        
        (if (and (== minimum 2) (== maximum 14))
         (= (conj ranks-as-integers 1) '(1 2 3 4 5 14)) 
         (= (range minimum (+ minimum 5)) ranks-as-integers))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
       check-pairs  (filter (fn [x] ((first x) hand)) checkers)
       values (map (fn [x] (second x)) check-pairs)
       maximum (apply max values)]
  maximum))

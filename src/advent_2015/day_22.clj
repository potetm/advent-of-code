(ns advent-2015.day-22
  (:import (clojure.lang PersistentQueue)))


(def spells
  #{{:spell/name :magic-missiles
     :spell/mana 53
     :spell/dmg 4
     :spell/heal 0}
    {:spell/name :drain
     :spell/mana 73
     :spell/dmg 2
     :spell/heal 2}
    {:spell/name :shield
     :spell/mana 113
     :spell/dmg 0
     :spell/heal 0
     :effect/armor 7
     :effect/duration 6}
    {:spell/name :poison
     :spell/mana 173
     :spell/dmg 0
     :spell/heal 0
     :effect/dmg 3
     :effect/duration 6}
    {:spell/name :recharge
     :spell/mana 229
     :spell/dmg 0
     :spell/heal 0
     :effect/mana 101
     :effect/duration 5}})


(def spells-idx
  (into {}
        (map (juxt :spell/name identity))
        spells))


(def init-state
  {:game/turn :player
   :game/effects []
   :game/winner nil
   :player/armor 0
   :player/hp 50
   :player/mana 500
   :player/mana-spent 0
   :boss/hp 51
   :boss/dmg 9})


(defn step [{t :game/turn
             es :game/effects
             gw :game/winner :as s}
            {m :spell/mana
             d :spell/dmg
             h :spell/heal
             sn :spell/name
             eff :effect/duration :as spell}]
  (if (or gw
          (some (comp #{sn} :spell/name)
                es))
    s
    (let [es' (into []
                    (remove #(= 1 (:effect/duration %)))
                    es)
          {php :player/hp
           pa :player/armor
           pm :player/mana
           pms :player/mana-spent
           bhp :boss/hp
           bdmg :boss/dmg :as s'} (reduce-kv (fn [s idx {m :effect/mana
                                                         d :effect/dmg
                                                         a :effect/armor}]
                                               (cond-> s
                                                 m (update :player/mana + m)
                                                 d (update :boss/hp - d)
                                                 a (assoc :player/armor a)

                                                 :always
                                                 (update-in [:game/effects
                                                             idx
                                                             :effect/duration]
                                                            dec)))
                                             (assoc s
                                               :game/effects es'
                                               :player/armor 0)
                                             es')]
      (case t
        :player
        (let [pm' (- pm m)
              php' (+ php h)
              bhp' (- bhp d)]
          (assoc s'
            :game/turn :boss
            :player/hp php'
            :player/mana pm'
            :player/mana-spent (+ pms m)
            :boss/hp bhp'
            :game/effects (if eff
                            (conj es' spell)
                            es')
            :game/winner
            (cond
              (not (pos? bhp'))
              :player

              (not (pos? pm'))
              :boss)))

        :boss
        (let [php' (- php
                      (max (- bdmg pa)
                           1))]
          (assoc s'
            :game/turn :player
            :player/hp php'
            :game/winner
            (cond
              (not (pos? bhp))
              :player

              (not (pos? php'))
              :boss)))))))


(defn traverse
  ([g]
   (traverse g step))
  ([{t :game/turn :as g} step]
   (let [helper (fn [g s]
                  (let [{gw :game/winner :as g'}
                        (step g s)]
                    (cond
                      gw [g']
                      (= g' g) nil
                      :else (traverse g' step))))]
     (if (= t :boss)
       (helper g nil)
       (mapcat (partial helper g)
               spells)))))


(defn part-1 []
  (:player/mana-spent (first (filter (comp #{:player}
                                           :game/winner)
                                     (traverse init-state)))))


(defn step2 [{t :game/turn :as g} s]
  (step (cond-> g
          (= t :player)
          (update :player/hp dec))
        s))

(defn traverse* [g]
  (loop [gs (sorted-set-by (fn [x y]
                             (compare (:player/mana-spent x)
                                      (:player/mana-spent y)))
                           g)]
    (let [{gw :game/winner
           t :game/turn :as g} (first gs)
          gs (disj gs g)]
      (cond
        (= gw :player) g
        (= gw :boss) (recur gs)
        (= t :boss) (recur (conj gs (step g nil)))
        :else
        (recur (into gs
                     (comp (map #(step g %))
                           (remove #{g}))
                     spells))))))

(defn part-2 []
  (traverse* init-state))

(comment
  (part-1)
  (part-2)

  )

user/tap-vals


(comment
  (sorted-set-by :player/mana-spent
                 )
  (def test1
    (assoc init-state
      :player/hp 10
      :player/mana 250
      :boss/hp 13
      :boss/dmg 8))
  (def test1-moves
    (interleave [:poison :magic-missiles]
                (repeat nil)))


  (def test2
    (assoc test1
      :boss/hp 14))
  (def test2-moves
    (interleave (map spells-idx [:recharge :shield :drain :poison :magic-missiles])
                (repeat nil)))




  (step (step init-state
              :poison)
        nil))

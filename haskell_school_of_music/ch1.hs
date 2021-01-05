module MyModule where
import Euterpea
import HSoM

mel1 = (note qn p1 :=: note qn (trans (-3) p1)) :+:
       (note qn p2 :=: note qn (trans (-3) p2)) :+:
       (note qn p3 :=: note qn (trans (-3) p3))
p1 = (Ef, 4)
p2 = (F, 4)
p3 = (G, 4)

mel2 = (note qn p1 :+: note qn p2 :+: note qn p3)
       :=:
       (note qn (trans (-3) p1) :+: note qn (trans (-3) p2) :+: note qn (trans (-3) p3))


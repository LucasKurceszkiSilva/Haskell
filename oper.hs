mult a 1 = a
mult _ 0 = 0
mult a b = a+(mult a (b-1))

pote a b = a ** b

tetr a 0 = 1
tetr a b = pote a (tetr a (b-1))

pent a 0 = 1
pent a b = tetr a (pent a (b-1))

hexa a 0 = 1
hexa a b = pent a (hexa a (b-1))

hept a 0 = 1
hept a b = hexa a (hept a (b-1))

octa a 0 = 1
octa a b = hept a (octa a (b-1))

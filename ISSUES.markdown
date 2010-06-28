sparking the right computation
------------------------------

Currently the computation of 'spec g f a' does exactly what I want
the computation of 'f g' happens in the spark queue through 'par'
while the computation of 'a' happens in the foreground, as well as
any (lazy) evaluation of 'f a'.

(TODO: flop the other of g and a in the cmp to force them in reverse order, to make g even lazier?)

However specSTM lacks this property. the 'par' has to launch the 
computation of 'a' because 'f g' is in STM. Can we play games with par  
to retain ownership of the STM transaction and move it into the par block, 
and rendezvous with it upon the completion of calculating 'a' ?

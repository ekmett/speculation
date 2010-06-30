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
and rendezvous with it upon the completion of calculating 'a' ? Per Marlow,
apparently not. This leads to a fallback plan:

sparking only when it makes sense
---------------------------------

Another option (from talking with Simon Marlow) is to add a new primop:

    numSparks# :: State# s -> (# Int#, State# s #)

which computes dequeElements(cap->spark) and to not speculate when that is too high.

Sadly, the cap and spark machinery is marked private by GHC, so this needs to become
a ticket.

unboxed tag-checking
--------------------

For tag checking purposes, we should be able to unsafeCoerce# a :: Word#, but it isn't subkinded. Ticket?

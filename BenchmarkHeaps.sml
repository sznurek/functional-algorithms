use "BinomialHeap.sml";
use "LeftistHeap.sml";

fun timeit f =
  let
    val timer = Timer.startRealTimer ()
    val start = Timer.checkRealTimer timer
    val _ = f ()
    val stop = Timer.checkRealTimer timer
  in
    (Time.toMicroseconds stop) - (Time.toMicroseconds start)
  end

fun randomList rand 0 = []
  | randomList rand n = Random.randNat rand :: randomList rand (n - 1)

fun binomial_sort xs =
  let
    val heap = List.foldr (fn (x, h) => BinomialIntHeap.insert (h, x)) BinomialIntHeap.empty xs
    fun sort h =
      if BinomialIntHeap.isEmpty h
        then []
        else BinomialIntHeap.findMin h :: sort (BinomialIntHeap.deleteMin h)
  in
    sort heap
  end

fun leftist_sort xs =
  let
    val heap = List.foldr (fn (x, h) => LeftistIntHeap.insert (h, x)) LeftistIntHeap.empty xs
    fun sort h =
      if LeftistIntHeap.isEmpty h
        then []
        else LeftistIntHeap.findMin h :: sort (LeftistIntHeap.deleteMin h)
  in
    sort heap
  end

fun testCase n = randomList (Random.rand (1,2)) n


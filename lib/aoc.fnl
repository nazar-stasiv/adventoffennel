(local lume (require :lib.lume))
(local fennel (require :fennel))

(fn string-from [path]
  "return lines read from file at path"
  (icollect [line (io.lines path)] line))

(fn bytes-from [path]
  "return bytes read from file at path"
  (with-open [r (io.open path "rb")]
    (r:read "a")))

(fn string-escape [s]
  "escapes characters of s"  
  (string.format "%q" s))

(fn string-last-index-of [s c]
  "returns index of c in s looking backwards or 0 if not found"
  (let [index (string.find (string.reverse s) c 1 true)]
    (if index (- (+ 1 (length s)) index)
        0)))

(fn string-indices [s c r ?j]
  "returns indices of char c in string s as table r"
  (let [i (string.find s c (or ?j 1) true)]
    (when i
      (table.insert r i)
      (string-indices s c r (+ 1 i)))
    r))

(fn string-all-indices [s c]
  "returns xs array of char c indices of string s"
  (let [res []]
    (string-indices s c res)
    res))

(fn string-pushback [s]
  "return string with 1st character from s pushed to last position"
  (.. 
   (string.sub s 2 (length s))
   (string.sub s 1 1)))

(fn string-tonumarray [str]
  "return collection of numbers from string str"
  (icollect [s (string.gmatch str (.. "[^ ,\t]+"))] (tonumber s)))

(fn string-toarray [s]
  "return collection of characters from string s abc->[a b c]"
  (let [result []]
    (for [i 1 (length s)]
      (table.insert result (string.sub s i i)))
    result))

(fn string-ends-with [s c]
  "returns bool indicating if c is in last position of s"
  (if (= c (. (string-toarray s) (length s)))
      true
      false))

(fn string-starts-with [s c]
  "returns bool indicating if c is prefix for s"
  ;;(assert (>= (length s) (length c)))
  (let [n (string.sub s 1 (length c))]
    (= n c)))

(fn string-split [str sep]
  "return strings from str separated at occurrences of sep"
  (icollect [s (string.gmatch str (.. "[^" sep "]+"))] s))

(fn string-trim [s]
  "returns copy of a string without trailing whitespace"
  (string.match s "^%s*(%g*)%s*$"))

(fn string-trim2 [s]
  "returns copy of a string without trailing whitespace"
  (string.gsub (string.gsub s "^%s" "") "%s$" ""))

(fn string-shift [s n]
  "return string with chars shifted by n, like in Caesar's cipher"  
  (let [nn (% n 26)
        ss (string-toarray s)
        cc (icollect [_ c (ipairs ss)]
             (string.char (+ 96 (% (+ nn (- (string.byte c) 96)) 26))))]
    (table.concat cc "")))

(fn string-subs [a b c ?j]
  "returns (ind,newa) where ind index of b and newa string where b replaced with c; or (0,a) if not found"
  (let [i (string.find a b (or ?j 1) true)]
    (if (not i) (values 0 a)
        (values (+ i (# b))
                (.. (string.sub a 1 (- i 1))
                    c
                    (string.sub a (+ i (# b)) -1))))))

(fn array-to-number [xs]
  "return integer value represented as collection xs of its digits [1 2 3]->123"
  (var result 0)
  (each [_ x (ipairs xs)]
    (set result (+ (tonumber x) (* result 10))))
  result)

(fn math-sum [n]
  "return sum of range numbers from 1 to n"
  (var result 0)
  (for [i 1 n 1]
    (set result (+ result i)))
  result)

(fn math-pow [x i]
  "return exponent exp of value mant x i->xⁱ"
  (if (= i 0) 1
      (= i 1) x
      (* x (math-pow x (- i 1)))))

(fn pow [mant exp]
  "return power mant to exp"
  (if (= 0 exp) 1
      (= 1 exp) mant
      (faccumulate [res mant i 2 exp] (* res mant))))

(fn pown [x n]
  "return x to power n"
  (^ x n))

(fn math-gcd [a b]
  "return greatest common denominator of a and b: 12,9->3"
  (if (= 0 b) a
      (math-gcd b (% a b))))

(fn int [x]
  "return integer representation of x"
  (or (tonumber x)
      (math.tointeger x)))

(fn xor [b1 b2]
  "boolean exclusive or either b1 or b2, but not both"
  (or
   (and b1 (not b2))
   (and (not b1) b2)))

(fn modulo+ [a b mod]
  "returns a+b modulo mod"
  (assert (<= a mod))
  (assert (<= b mod))
  (let [res (+ a b)]
    (if (<= res mod) res
        (- res mod))))

(fn mod [x y]
  "returns x modulo y"
  (% x y))

(fn math-lcm [a b]
  "return least common multiplier of a and b"
  (int (/ (* a b) (math-gcd a b))))

(fn math-fact [n]
  "return factorial of n"
  (if (<= n 1) 1
      (* n (math-fact (- n 1)))))

(fn math-min [t]
  "return smallest element from collection t"
  (table.sort t)
  (. t 1))

(fn math-max [t]
  "return largest element from collection t"
  (table.sort t)
  (. t (# t)))

(fn min-index [xs]
  "return index of smallest element from collection xs"
  (var k 1)
  (var v (. xs 1))
  (for [i 2 (# xs)]
    (when (< (. xs i) v)
      (set k i)
      (set v (. xs i))))
  k)

(fn max-index [xs]
  "return index of largest element from collection xs"
  (var k 1)
  (var v (. xs 1))
  (for [i 2 (# xs)]
    (when (> (. xs i) v)
      (set k i)
      (set v (. xs i))))
  k)

(fn take [xs n]
  "return first n elements from collection xs"
  (let [result []]
    (for [i 1 n 1]
      (table.insert result (. xs i)))
    result))

(fn take-n [n xs]
  "return first n elements from xs"  
  (fcollect [i 1 n] (. xs i)))

(fn rest [xs]
  "return elements from collection xs starting at index two till end"
  (table.remove xs 1)
  xs)

(fn table-print [xs]
  "print to output elements of xs"
  (print (fennel.view xs)))

(fn table-sum [xs]
  "return sum of elements at xs collection"
  (accumulate [sum 0 _ x (ipairs xs)]
    (if (lume.isarray x)
        (+ sum (table-sum x))
        (+ sum x))))

(fn table-prod [xs]
  "return product of xs elements"
  (accumulate [prod 1 _  x (ipairs xs)]
    (* prod x)))

(fn table-filter [xs x]
  "return array with elements of xs s.t. xsᵢ=x"
  (lume.filter xs #(= x (tonumber $))))

(fn table-count [xs x]
  "return number of x in xs"
  (length (table-filter xs x)))

(fn table-sort [xs ?f]
  "return table sorted in ascending order"
  (if ?f
      (table.sort xs ?f)
      (table.sort xs))
  xs)

(fn table-join [xs ys]
  "return collection with all ys elements appended to all xs elements"
  (if ys
      (table.move ys 1 (length ys) (+ 1 (length xs)) xs)
      xs))

(fn table-join3 [xs ys zs]
  "return table with ys, zs elements elements appended to xs elements"
  (let [ws (table-join xs ys)]
    (table-join ws zs)))

(fn table-contains? [t e]
  "return bool indicating if collection t contains element e"
  (if (lume.find t e)
      true
      false))

(fn table-identical-2d? [a b]
  "return bool indicating if 2D collection a has identical elements to b"
  (var result true)
  (for [i 1 (length a) 1]
    (for [j 1 (length (. a i)) 1]
      (when (not= (. (. a i) j)
                  (. (. b i) j))
        (set result false))))
  result)

(fn table-identical? [t1 t2]
  "return bool indicating all t1 elements present in t2 collection"
  (and 
   (= (length t1)
      (length t2))
   (lume.all t1 #(table-contains? t2 $))))

(fn table-equals [xs ys]
  "indicates both xs and ys hold identical elements in the same order"
  (var done false)
  (if (not= (# xs) (# ys))
      (set done true)
      (for [i 1 (# xs) &until done]
        (when (not= (. xs i) (. ys i))
          (set done true))))
  (not done))

(fn table-zip [t1 t2]
  "return collection of tuples: 1st t1 element, 1st t2 element, then 2nd t1 and 2nd t2, and so on"
  (assert (= (length t1) (length t2)))
  (let [result []]
    (for [i 1 (length t1) 1]
      (when (. t2 i)
        (table.insert result [(. t1 i) (. t2 i)])))
    result))

(fn table-unzip [xys]
  "convert tuple array into array of x and array of y elements"
  (let [xs (icollect [_ v (ipairs xys)] (. v 1))
        ys (icollect [_ w (ipairs xys)] (. w 2))]
    [xs ys]))

(fn table-reverse [xs]
  "return collection with xs elements reversed"
  (let [result []]
    (each [_ x (ipairs xs)]
      (table.insert result 1 x))
    result))

(fn matrix-reverse [m]
  "return matrix with m rows reversed"
  (lume.map m #(table-reverse $)))

(fn table-reset [t]
  "removes in place any elements from table t"
  (while (< 0 (length t))
    (table.remove t))
  t)

(fn table-remove [t v]
  "removes in place element v if exists in t"
  (let [i (lume.find t v)]
    (when i
      (table.remove t i)))
  t)

(fn table-zero? [t]
  "return bool indicating if all t elements are zeros"
  (and (< 0 (length t))
       (lume.all t #(= 0 $))))

(fn table-range [xs f t]
  "return collection of xs elements starting at index f ending at index t"
  (let [result []]
    (fcollect [i f t 1] (table.insert result (. xs i)))
    result))

(fn table-apply [t1 t2 f]
  "return collection of (f t1 t2) applications to elements of t1 and t2"
  (assert (= (length t1)
             (length t2)))
  (let [result []]
    (for [i 1 (length t1) 1]
      (table.insert result (f (. t1 i) (. t2 i))))
    result))

(fn table-clone [xs]
  "return shallow copy of xs elements"
  (let [res []]
    (each [_ x (pairs xs)]
      (table.insert res x))
    res))

(macro times [t body1 & rest-body]
  `(fcollect [i# 1 ,t 1]
     (do ,body1 ,(unpack rest-body))))

(macro time [body1 & rest-body]
  `(let [start# (os.clock)]
     ,body1
     ,(unpack rest-body)
     (print (.. "Elapsed, s " (- (os.clock) start#)))))

(macro swap [a b & body1]
  `(let [tmp# ,a]
     (set ,a ,b)
     (set ,b tmp#)
     ,(unpack body1)))

(macro apply [fun args]
  `(,fun ,((or table.unpack _G.unpack) args)))

(macro memfn1 [fun args & body]
  `(do
     (var ,fun nil)
     (set ,fun (lume.memoize (fn ,args ,(unpack body))))))

(macro memfn2 [fun args & body]
  `(fn ,fun ,args (lume.memoize (fn ,args ,(unpack body)))))

(fn table-group-by [xs n]
  "return n-ary collection of linear xs"
  (assert (= 0 (% (length xs) n)))
  (let [result []
        in (table-clone xs)]
    (while (not= 0 (length in))
      (table.insert result (take in n))
      (times n (rest in)))
    result))

(fn table-transpose [xs]
  "return rows of 2D collection xs as columns"
  (let [result []]
    (for [j 1 (length (. xs 1)) 1]
      (table.insert result j [])
      (for [i 1 (length xs) 1]
        (table.insert (. result j) i (. (. xs i) j))))
    result))

(fn table-column-sum [xs]
  "return sum of columns of 2d array xs"
  (let [ys (table-transpose xs)]
    (lume.map ys #(table-sum $))))

(fn table-replace [t i j v]
  "return collection with i,j element of t replaced with v"
  (let [old (table.remove (. t i) j)]
    (table.insert (. t i) j v)
    old))

(fn table-replace-row [xs i x]
  "return ith row of xs replaced in-place with x"
  (let [old (table.remove xs i)]
    (table.insert xs i x)
    old))

(fn table-swap [xs i v]
  "return xs with ith element replaced by v"
  (table.remove xs i)
  (table.insert xs i v)
  xs)

(fn table-index-swap [t i1 i2]
  "in-place swap of (. t i1) and (. t i2)"
  (let [a (. t i1)
        b (. t i2)]
    (table.remove t i1)
    (table.insert t i1 b)
    (table.remove t i2)
    (table.insert t i2 a)
    t))

(fn table-prod-kv [xs]
  "return product of xs value by its 0-based index"
  (icollect [k v (ipairs xs)] (* (- k 1) v)))

(fn permutation [xs len res]
  "returns possible permutations of xs elements as 2d array res"
  (if (= len 0)
      (table.insert res (table-clone xs))
      (for [i 1 len]
        (table-index-swap xs i len)
        (permutation xs (- len 1) res)
        (table-index-swap xs i len)))
  res)

(lambda table-move [pos xs1 xs2 ?n]
  "moves element(s) at pos from xs1 into same pos at collection xs2"
  (if (not ?n)
      (table.insert xs2 pos (table.remove xs1 pos))
      (let [tmp []]
        (for [i 1 ?n]
          (table.insert tmp 1 (table.remove xs1 pos)))
        (for [i 1 ?n]
          (table.insert xs2 pos (table.remove tmp 1)))))
  nil)

(fn string-totable [line]
  "converts space separated key:value string into table"
  (let [tokens (string-split line " ")
        res {}]
    (each [_ token (ipairs tokens)]
      (let [index (string.find token ":")]
        (tset res
              (string.sub token 1 (- index 1))
              (string.sub token (+ index 1)))))
    res))

(fn table-tostring [xs]
  "joins elements of xs with empty string"
  (if xs
      (table.concat xs "")
      ""))

(fn matrix-print [xs]
  "prints matrix with ascii == or space if zero"  
  (each [i x (ipairs xs)]
    (print
     (string.format
      "%s" (.. 
            (table-tostring
             (lume.map x #(if (= "1" (tostring $)) "#" "."))))))))

(fn table-unique [t]
  "returns unique set of [x y] elements of t"
  (let [res []]
    (each [_ e (ipairs t)]
      (let [exists (lume.filter res
                                (fn [[x y]]
                                  (and (= x (. e 1))
                                       (= y (. e 2)))))]
        (when (or (= exists nil)
                  (= 0 (length exists)))
          (table.insert res e))))
    res))

(fn head [xs]
  "return element from collection xs at index of one"  
  (. xs 1))

(fn tail [xs]
  "return elements from xs from index 2 to index #xs"
  (table-range xs 2 (# xs)))

(fn not-empty? [xs]
  "return bool indicating if xs contains any elements"  
  (and (not= nil xs) (< 0 (# xs))))

(fn first [xs]
  "return element from collection xs at index of one"
  (. xs 1))

(fn last [xs]
  "return last element from collection xs"
  (. xs (length xs)))

(fn fold [t]
  "return sum of values of t"
  (lume.reduce t (fn [acc x] (+ acc x))))

(fn empty? [xs]
  "return bool indicating if xs contains any elements or is empty"
  (or (= nil xs)
      (= 0 (length xs))))

(fn table-contains-2d? [t e]
  "return bool indicating if collection t contains table e"
  (let [xs (lume.filter t #(table-equals $ e))]
    (not (empty? xs))))

(fn matrix-contains? [m vec]
  "returns bool if any of m rows identical with vec"
  (let [match-vec (fn [v1 v2]
                    (var res true)
                    (for [i 1 (# v1)]
                      (when (not= (. v1 i) (. v2 i))
                        (set res false)))
                    res)]
    (lume.match m #(match-vec $ vec))))

(fn range [s len]
  "return collection of consecutive numbers starting at s till length len"
  (if (< len 1) [] 
      (fcollect [i s (+ s len) 1] i)))

(fn range-to [s e]
  "return collection of consecutive numbers starting at s ending at e"
  (fcollect [i (math.min s e) (math.max s e) 1] i))

(fn range-step [f l s]
  "return numbers starting at f ending at l with step s"
  (fcollect [i (math.min f l) (math.max f l) s] i))

(fn range-of [x n]
  "return length n collection of x"
  (let [res []]
    (for [i 1 n]
      (table.insert res x))
    res))

(fn tonum [s]
  "returns int or ascii code if s represents letter rather than a number"
  (if s
      (or (tonumber s) (string.byte s))
      0))

(fn todecimal [t]
  "returns decimal number of bitarray: [1 1 1 1]->15"
  (let [res []
        len  (length t)]
    (each [i v (ipairs t)]
      (table.insert res (* v (math-pow 2 (- len i)))))
    (table-sum res)))

(fn dec [x]
  "return 1 decrement of x"
  (- x 1))

(fn inc [x]
  "return 1 increment of x"
  (+ x 1))

(fn new-matrix [row col x]
  "return row by col 2d array of x"  
  (let [res []]
    (for [i 1 row]
      (table.insert res (range-of x col)))
    res))

(fn matrix [x y v]
  "return x by y matrix of v"
  (let [res []]
    (for [i 1 y]
      (table.insert res (fcollect [i 1 x 1] v)))
    res))

(fn read-matrix [xs ?n]
  "return 2d array of characters"
  (let [res []]
    (each [_ x (ipairs xs)]
      (let [el (string-toarray x)]
        (if ?n (table.insert res (lume.map el #(tonumber $)))
            (table.insert res el))))
    res))

(fn write-matrix [xs out]
  "writes pbm portable bitmap, caller closes io.out"
  (let [M (# (. xs 1))
        N (# xs)]
    (out.write out (.. "P1\n" M " " M "\n"))
    (for [i 1 N]
      (let [y (/ (- N (* 2 i)) N)]
        (for [j 1 M]
          (let [x (/ (- (* j 2) M) M)
                pt (?. (?. xs y) x)]
            (out.write out (if pt "1" "0"))))))
    (out.write out "\n"))
  out)

(fn matrix-swap [m x y v]
  "set [x y] element of 2d array m to v"
  (let [row (. m y)]
    (table.remove row x)
    (table.insert row x v)))

(fn matrix-set [m x1 y1 x2 y2 v]
  "set range [x1 y1] to [x2 y2] of 2d array m to v"  
  (for [i y1 y2]
    (for [j x1 x2]
      (matrix-swap m j i v))))

(fn matrix-toggle [m x1 y1 x2 y2]
  "toggle range [x1 y1] to [x2 y2] of 2d array m to either 0 or 1"
  (for [i y1 y2]
    (for [j x1 x2]
      (if (= 0 (. (. m i) j))
          (matrix-swap m j i 1)
          (matrix-swap m j i 0)))))

(fn matrix-apply [m x1 y1 x2 y2 f]
  "apply f to range [x1 y1] to [x2 y2] of 2d array m and update"
  (for [i y1 y2]
    (for [j x1 x2]
      (matrix-swap m j i (f (. (. m i) j))))))

(fn matrix-clone [xs]
  "return copy of 2d array xs"
  (let [res []]
    (each [_ x (pairs xs)]
      (table.insert res (table-clone x)))
    res))

(fn matrix-adjvals [xs i j]
  "returns array of [j,i] neighbour vals of 2d array xs"
  (let [adj-idx
        [[-1 -1] [-1 0] [-1 1]
         [0 -1]         [0 1]
         [1 -1] [1 0] [1 1]]]
    (icollect [_ [di dj] (ipairs adj-idx)]
      (?. (?. xs (+ i di)) (+ j dj)))))

(fn matrix-match-count [m s]
  "returns number of matches of table s in matrix m up down left right and diagonally; s length limited to 4"
  (assert (= 4 (# s)))
  (var res 0)
  (for [i 1 (# m)]
    (for [j 1 (# (. m i))]
      (when (= (. s 1) (. (. m i) j))
        (let [pos [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
          (each [_ [di dj] (ipairs pos)]
            (when (and (= (. s 2) (?. (?. m (+ i di)) (+ j dj)))
                       (= (. s 3) (?. (?. m (+ i di di)) (+ j dj dj)))
                       (= (. s 4) (?. (?. m (+ i di di di)) (+ j dj dj dj))))
              (set res (+ 1 res))))))))
  res)

(fn matrix-find [xs s]
  "returns [x y] pos of s in 2d table xs"
  (var done false)
  (for [i 1 (# xs) &until done]
    (for [j 1 (# (. xs i)) &until done]
      (when (= s (. (. xs i) j))
        (set done [j i]))))
  done)

(fn matrix-count [xs s]
  "returns count of s in 2d table xs"
  (var done 0)
  (for [i 1 (# xs)]
    (for [j 1 (# (. xs i))]
      (when (= s (. (. xs i) j))
        (set done (+ 1 done)))))
  done)

(fn matrix-index [xs x y]
  "return value of xs at index [x y]"
  (if (or (= nil x)
          (= nil y)
          (not (<= 1 x (# (. xs 1))))
          (not (<= 1 y (# xs))))
      nil
  (. (. xs y) x)))

(fn matrix-print [xs]
  (lume.map xs #(print (table.concat $ ""))))

(fn table-tonumber [xs]
  "returns elements of table converting to numbers"
  (lume.map xs #(tonumber $)))

(fn table-unpack [xs]
  "returns elements of table"
  (let [u (or table.unpack _G.unpack)]
    (u xs)))

(fn powerset [xs]
  "returns every possible combination of xs elements"
  (let [res [[]]]
    (for [i 1 (# xs)]
      (for [j 1 (# res)]
        (table.insert res [(. xs i) (table-unpack (. res j))])))
    res))

(fn combination [xs i]
  "returns every i element combination of length n xs i.e. Cⁱₙ"
  (let [ys (powerset xs)]
    (lume.filter ys (fn [y] (= i (# y))))))

(fn max [xs]
  "return maximum element of xs"  
  (math.max (table-unpack xs)))

(fn min [xs]
  "return minimum element of xs"
  (math.min (table-unpack xs)))

(fn table-disjunc [xxs]
  "return common elements of xxs₁ .. xxsₙ tables elements i.e. disjunction set"
  (if (= 1 (length xxs))
      (. xxs 1)
      (= 0 (length xxs))
      nil
      (let [res []
            t1 (. xxs 1)
            t2 (. xxs 2)]
        (each [_ e1 (ipairs t1)]
          (when (lume.find t2 e1)
            (table.insert res e1)))
        (table-disjunc [res (table-unpack (table-range xxs 3 (length xxs)))]))))

(fn table-min [xs]
  "return minimum entry of xs"
  (math.min (table-unpack (table-tonumber xs))))

(fn table-max [xs]
  "return maximum entry of xs"
  (let [xn (table-tonumber xs)]
    (if (< 8000 (length xn))
        (do (table.sort xn)
            (. xn (length xn)))
        (math.max (table-unpack xn)))))

(fn table-exclude [xs j]
  "returns all xs elements but at pos j"
  (let [ys (table-clone xs)]
    (table.remove ys j)
    ys))

(fn table-exclude-all [xs js]
  "returns all xs elements but at indices specified with js"
  (icollect [i v (ipairs xs)]
    (if (lume.find js i) nil v)))

(fn table-flatten [xs]
  "returns linear collection of xs rows"  
  (let [res []]
    (each [_ x (ipairs xs)]
      (each [_ y (ipairs x)]
        (table.insert res y)))
    res))

(fn table-flatten2 [xs]
  "returns linear collection of n-dimension table xs"
  (let [res []]
    (each [_ x (ipairs xs)]
      (if (lume.isarray x)
          (lume.map (table-flatten2 x) #(table.insert res $))
          (table.insert res x)))
    res))

(fn table-union [xs]
  "return all elements of xs₁ .. xsₙ tables elements i.e. conjunction set"  
  (lume.unique (table-flatten xs)))

(fn table-sum-if [xs pred]
  "return sum of xs elements if xᵢ satisfies pred condition"
  (accumulate [sum 0 _ x (ipairs xs)]
    (if (lume.isarray x)
        (+ sum (table-sum-if x pred))
        (if (pred x)
            (+ sum x)
            (+ sum 0)))))

(fn runtime []
  "returns lua runtime version"
  (case _G._VERSION
    "Lua 5.1" 5.1
    "Lua 5.2" 5.2
    "Lua 5.3" 5.3
    "Lua 5.4" 5.4))

(fn int/ [x y]
  "returns integer division like // operator"
  (math.floor (/ x y)))

(fn concat [x y]
  "returns concatenated xy integer"
  (tonumber (string.format "%d%d" x y)))

(fn tolong [s]
  "converts sci notation s i.e. 1.0551712821154e+14->10551712821154"
  (string.format "%d" s))

(fn toarray [n]
  "return 6-digit number as digit array"
  [(% (int/ n 100000) 10)
   (% (int/ n 10000) 10)
   (% (int/ n 1000) 10)
   (% (int/ n 100) 10)
   (% (int/ n 10) 10)
   (% n 10)])

(fn lazy-seq [xs f]
  "return applications f to tail of xs until f(xs) converges to 0"
  (var res (f (. xs (length xs))))
  (while (< 0 res)
    (table.insert xs res)
    (set res (f (. xs (length xs)))))
  xs)

(fn intersection [[[a0 a1] [b0 b1]] [[c0 c1] [d0 d1]]]
  "find [x y] where segments [a0 a1][b0 b1] and [c0 c1][d0 d1] cross"  
  (let [td (- (* (- a0 c0) (- c1 d1))
              (* (- a1 c1) (- c0 d0)))
        ud (- (* (- a0 c0) (- a1 b1))
              (* (- a1 c1) (- a0 b0)))
        d (- (* (- a0 b0) (- c1 d1))
             (* (- a1 b1) (- c0 d0)))]
    (if (= 0 d) nil
        (and (<= 0 (/ td d) 1)
             (<= 0 (/ ud d) 1))
        [(+ a0 (* (/ td d) (- b0 a0)))
         (+ a1 (* (/ td d) (- b1 a1)))]
        nil)))

(fn manhattan-dist [a b]
  "return taxicab distance between [x1 y1] and [x2 y2] on a plane"
  (let [[x1 y1] a [x2 y2] b]
    (+ 
     (math.abs (- x1 x2))
     (math.abs (- y1 y2)))))

(fn hamming-dist [s1 s2]
  "returns number of chars s1 differs from s2"
  (assert (= (length s1)
             (length s2)))
  (var res 0)
  (let [xs1 (string-toarray s1)
        xs2 (string-toarray s2)]
    (each [i xi (ipairs xs1)]
      (when (not= (. xs2 i) xi)
        (set res (+ 1 res)))))
  res)

(fn decartian [directions]
  "returns [[0 0] [x1 y1]...[xn yn]] coords for RX,UX,LX,DX directions"
  (let [xs (string-split directions ",")
        res [[0 0]]]
    (each [_ xx (ipairs xs)]
      (let [[x0 y0] (. res (length res))]
        (case [(string.sub xx 1 1) (tonumber (string.sub xx 2 (length xx)))]
          ["U" y] (table.insert res [x0 (+ y0 y)])
          ["D" Y] (table.insert res [x0 (- y0 Y)])
          ["L" X] (table.insert res [(- x0 X) y0])
          ["R" x] (table.insert res [(+ x0 x) y0]))))
    res))

(fn in-segment? [[x0 y0] [[x1 y1] [x2 y2]]]
  "true if pt [x0 y0] inside of a [x1 y1][x2 y2] line on a 2d plane"
  (or (and (<= x1 x0 x2) (<= y1 y0 y2))
      (and (<= x2 x0 x1) (<= y1 y0 y2))
      (and (<= x1 x0 x2) (<= y2 y0 y1))
      (and (<= x2 x0 x1) (<= y2 y0 y1))))

(fn partition-by [xs f]
  "groups xs elements by predicate f e.g. identity"
  (let [res [[(. xs 1)]]]
    (for [i 2 (length xs)]
      (if (f (. xs i)
             (last (. res (length res))))
          (table.insert (. res (length res)) (. xs i))
          (table.insert res [(. xs i)])))
    res))

(fn partition-identity [xs]
  "groups xs elements by identity"
  (let [identity (fn [a b] (= a b))]
    (partition-by xs identity)))

(fn partition-at [xs delim]
  "partitions xs into 2d array at each delim occurence"
  (var cur [])
  (let [res []]
    (each [_ x (ipairs xs)]
      (if (= delim x)
          (do
            (table.insert res cur)
            (set cur []))
          (table.insert cur x)))
    (table.insert res cur)
    res))

(fn partition1 [xs]
  "partitions xs elements into pairs with step one"
  (let [res []]
    (for [i 2 (length xs)]
      (table.insert res [(. xs (- i 1)) (. xs i)]))
    res))

(fn partition3step1 [xs]
  "partitions xs elements into triads with step one"
  (let [res []]
    (for [i 3 (length xs)]
      (table.insert res [(. xs (- i 2)) (. xs (- i 1)) (. xs i)]))
    res))

(fn partition2 [xs]
  "partitions xs elements into pairs with step two, pads with nil"
  (let [res []]
    (for [i 2 (length xs) 2]
      (table.insert res [(. xs (- i 1)) (. xs i)])
      (when (= 1 (- (length xs) i))
        (table.insert res [(. xs (+ i 1)) nil])))
    res))

(fn partition3 [xs]
  "partitions xs elements into triads, pads with nil"
  (let [res []
        len (# xs)]
    (for [i 1 len 3]
      (case (- len i)
        0 (table.insert res [(. xs i) nil nil])
        1 (table.insert res [(. xs i) (. xs (+ i 1)) nil])
        _ (table.insert res [(. xs i) (. xs (+ i 1)) (. xs (+ i 2))])))
    res))

(fn partitionm [xs m]
  "partitions xs by m elements step 1"
  (assert (<= 1 m (# xs)))
  (if (= 1 m) (lume.map xs (fn [x] [x]))
      (let [res []]
        (for [i m (length xs)]
          (table.insert
           res
           (lume.map (range 0 (- m 1))
                     #(. xs (- i $)))))
        res)))

(fn table-no-dups? [xs]
  "returns true if xs has no duplicate elements"
  (table.sort xs)
  (var res true)
  (let [xy (partition1 xs)]
    (each [_ [x y] (ipairs xy)]
      (when (= x y)
        (set res false))))
  res)

(fn frequency [xs]
  "returns xs element with most duplicates as xs subarray"
  (table.sort xs)
  (let [ys (partition-by xs #(= $1 $2))
        pred (fn [a b] (< (# a) (# b)))]
    (table.sort ys pred)
    (. ys (length ys))))

(fn rank [xs k]
  "returns number of lookups in xs starting from key k"
  (let [xsk (. xs k)]
    (if (not xsk) 0
        (lume.isarray xsk)
        (max (icollect [k v (ipairs xsk)]
               (+ 1 (rank xs v))))
        (+ 1 (rank xs xsk)))))

(fn adjacency-list [xs]
  "hashmap representation of a graph {:vert1 [:vert2...] :vert2...}"
  (let [res {}]
    (each [i [f t] (ipairs xs)]
      (if (= 1 i) (tset res f [t])
          (. res f) (table.insert (. res f) t)
          (do (tset res f [t])
              (tset res t []))))
    res))

(fn keys [xs]
  "returns keys of a hash-map xs"
  (icollect [k v (pairs xs)] k))

(fn vals [xs]
  "returns values of a hash-map xs"
  (icollect [k v (pairs xs)] v))

(fn adjacency-root [list]
  "returns key of adjacency list with no input edges"
  (let [xs (lume.map (keys list)
                     (fn [e] [e (rank list e)]))]
    (table.sort xs #(< (. $1 2) (. $2 2)))
    (. (. xs (# xs)) 1)))

(fn collide? [box1 box2]
  "indicates if rectangle box1 defined as  [x y w h] and box2 overlap"
  (and (< (* 2 (math.abs (- (. box1 1) (. box2 1))))
          (+ (. box1 3) (. box2 3)))
       (< (* 2 (math.abs (- (. box1 2) (. box2 2))))
          (+ (. box1 4) (. box2 4)))))

(fn dist2rd [[Hx Hy] {:x Tx :y Ty}]
  "x₁y₁ distance² to x₂y₂ on plane <=2 for any adjacent points"
  (lume.distance Hx Hy Tx Ty true))

(fn qpush [xs x]
  "queue version of push function"
  (table.insert xs x))

(fn qpop [xs]
  "queue version of pop function"
  (table.remove xs 1))

(fn qpeek [xs]
  "queue version of peek function"
  (. xs 1))

(fn push [xs x]
  "stack version of push function"
  (table.insert xs x))

(fn pop [xs]
  "stack version of pop function"
  (table.remove xs))

(fn peek [xs]
  "stack version of peek function"
  (. xs (# xs)))

(fn ppush [xs v p]
  "priority queue version of push function"  
  (if (. xs p)
      (table.insert (. xs p) v)
      (tset xs p [v]))
  xs)

(fn ppop [xs]
  "priority queue version of pop function"
  (let [keys (keys xs)]
    (table.sort keys)
    (var key (. keys (# keys)))
    (var val (. xs key))
    (tset xs key nil)
    val))

(fn odd? [x]
  "returns true if x is odd integer"
  (= 1 (% x 2)))

(fn even? [x]
  "returns true if x is even integer"  
  (not (odd? x)))

(fn table-odd [xs]
  "returns xs elements if element index is odd"
  (icollect [k v (ipairs xs)]
    (if (= 1 (% k 2)) v nil)))

(fn table-even [xs]
  "returns xs elements if element index is even"
  (icollect [k v (ipairs xs)]
    (if (= 0 (% k 2)) v nil)))

(fn table-update [t k1 k2 v]
  "Updates (. t k1) with {:k2 v} preserving (. t k1) value if exists"
  (let [st (?. t k1)]
    (if (= nil st)
        (tset t k1 {k2 v})
        (tset st k2 v))))

(fn table-upsert [t k v]
  "returns t with {:k [v]} preserving (. t k) existing value"
  (let [xs (. t k)]
    (if xs
        (table.insert xs v)
        (tset t k [v])))
  t)

(fn table-mid [xs]
  "returns xᵢ of xs where i is median index"
  (assert (odd? (# xs)))
  (. xs (math.ceil (/ (# xs) 2))))

(fn html [tag xs]
  "return tag with attributes (. xs :at) and children (. xs :ch)"
  (.. "\n<"
      tag
      (if (. xs :at)
          (accumulate [attrs " " k v (pairs (. xs :at))]
            (.. attrs " " k "=" "\"" v "\"")) "")
      ">"
      (if (. xs :ch)
          (accumulate [ch "" _ c (ipairs (. xs :ch))]
            (.. ch  (case (type c)
                      :nil ""
                      :string c
                      :table (accumulate [children "" k v (pairs c)]
                               (.. children (html k v)))
                      _ (tostring c)))) "")
      "\n</"
      tag
      ">"))

(fn turtle-new [width height]
  "returns svg canvas for turtle graphics"
  (let [image
        {:at {:version "1.1"
              :width (tostring width)
              :height (tostring height)
              :xmlns "http://www.w3.org/2000/svg"}
         :ch [{:rect {:at {:width "100%" :height "100%" :fill "white"}}}]}]
    {:width width :height height :image image
     :drawing true :point [1 1] :angle 0 :color "black"}))

(fn turtle-pen-up? [turtle]
  "returns boolean if turtle graphics pen is in up position"
  (not (. turtle :drawing)))

(fn turtle-pen-up [turtle]
  "lifts turtle graphics pen up"
  (tset turtle :drawing false)
  turtle)

(fn turtle-pen-down [turtle]
  "sets turtle graphics pen in drawing position"
  (tset turtle :drawing true)
  turtle)

(fn turtle-pen-color [turtle color]
  "Set pen color to black, silver, gray, white, maroon, red, purple, fuchsia, green, lime, olive, yellow, navy, blue, teal, aqua"
  (tset turtle :color color)
  turtle)

(fn turtle-right [turtle ?degrees]
  "move turtle graphics pen to the right"
  (let [angle (. turtle :angle)]
    (tset turtle :angle (% (+ angle (or ?degrees 90)) 360)))
  turtle)

(fn turtle-left [turtle ?degrees]
  "move turtle graphics pen to the left"
  (turtle-right turtle (- (or ?degrees 90)))
  turtle)

(fn turtle-draw-line [turtle p1 p2]
  "draw line with turtle graphics pen"
  (let [image (. turtle :image)
        color (. turtle :color)]
    (table.insert (. image :ch)
                  {:line {:at {:x1 (. p1 1)
                               :x2 (. p2 1)
                               :y1 (. p1 2)
                               :y2 (. p2 2)
                               :stroke "black"
                               :stroke-width 5}}}))
  turtle)

(fn turtle-forward [turtle steps]
  "move turtle graphics pen forward steps"
  (let [p1 (. turtle :point)
        angle  (. turtle :angle)
        p2 [(+ (. p1 1) (* (math.sin (* angle (/ math.pi 180))) steps))
            (+ (. p1 2) (* (math.cos (* angle (/ math.pi 180))) steps))]]
    (when (. turtle :drawing)
      (turtle-draw-line turtle p1 p2))
    (tset turtle :point p2))
  turtle)

(fn turtle-back [turtle steps]
  "move turtle graphics pen backward steps"
  (turtle-forward turtle (- steps))
  turtle)

(fn turtle-go [turtle x y]
  "move turtle graphics pen to pos x,y"
  (let [p1 (. turtle :point) p2 [x y]]
    (when (. turtle :drawing)
      (turtle-draw-line turtle p1 p2))
    (tset turtle :point p2))
  turtle)

(fn turtle-toward [turtle x y]
  "move turtle graphics pen to pos x,y"
  (let [p (. turtle :point)
        polar (math.atan (- (. p 1) x) (- (. p 2) y))
        angle (% (/ polar (/ math.pi 180)) 360)]
    (tset turtle :angle angle))
  turtle)

(fn turtle-distance [turtle x y]
  "return taxicab distance pos x,y to turtle graphics pen pos"
  (let [p (. turtle :point)]
    (math.sqrt
     (+ (^ (- (. p 1) x) 2)
        (^ (- (. p 2) y) 2)))))

(fn turtle-write [turtle name]
  "writes turtle graphics canvas to svg file"
  (let [out (io.open name "w")
        image (. turtle :image)]
    (out.write out (html :svg image))
    (out.close out))
  turtle)

(fn prob-bayes [baserate newevidence]
  "Given base-rate, how strong is new evidence"
  (/ newevidence
     (+ newevidence
        (* (- 1 newevidence)
           (/ (- 1 baserate) baserate)))))

(fn prob-mean [xs]
  "what is mean value for xs elements"
  (var res 0)
  (let [n (length xs)]
    (each [_ v (ipairs xs)]
      (set res (+ res v)))
    (/ res n)))

(fn prob-variation [xs]
  "what is deviation² for xs elements"
  (var res 0)
  (let [avg (prob-mean xs)]
    (each [_ x (ipairs xs)]
      (set res (+ res (^ (- x avg) 2)))))
  (/ res (- (length xs) 1)))

(fn prob-dist-binom [p n i]
  "Given infection rate 20%, chance to shake hands with less than 3 infected people at 40-hands meeting is (prob-dist-binom 0.2 40 3)"
  (faccumulate [res 0 j 0 (- i 1)]
    (+ res (* (^ p j) (^ (- 1 p) (- n j))))))

(fn prob-dist-geom [p i]
  "given 3 dice rolls, chance of 6 is (prob-dist-geom (/ 1 6) 3)"
  (faccumulate [res p j 1 (- i 1)]
    (+ res (* p (^ (- 1 p) j)))))

(fn prob-nchoosei [n i]
  "Number of sets of i out of n elements is (prob-nchoosei n i)"
  (/ (math-fact n)
     (* (math-fact i)
        (math-fact (- n i)))))

(fn prob-binom [n i p]
  "chance for a couple to have 3 daughters out of 5 children is (prob-binom 5 3 0.5); With 0.05 zombie infection rate, chance to get through 20 people to the shelter door and surviving at most 2 contacts is (+ (prob-binom 20 0 0.05) (prob-binom 20 1 0.05) (prob-binom 20 2 0.05))"
  (* (prob-nchoosei n i)
     (^ p i)
     (^ (- 1 p) (- n i))))

(fn table-intersect [t1 t2]
  "Returns t1∩t2 - elements present in both t1 and t2"
  (let [result []]
    (each [_ v (ipairs t1)]
      (when (lume.find t2 v)
        (table.insert result v)))
    result))

(fn table-shift [xs v]
  "shift xsᵢ to the right by v s.t. ith element moves to i+v position"
  (let [res (table-clone xs)
        len (# xs)
        u (% v len)]
    (for [i (# res) 1 -1]
      (table-swap res i (. xs (modulo+ len (- i u) len))))
    res))

(fn table-index-loop [xs i]
  "get ith element of xs as if xs is an infinite seq with a cycle"
  (let [len (# xs)
        j (if (<= i len) i
              (= 0 (% i len)) len
              (% i len))]
    (. xs j)))

(fn ulam-spiral [n2]
  "17  16  15  14  13"
  "18   5   4   3  12"
  "19   6   1   2  11"
  "20   7   8   9  10"
  "21  22  23---> ..."
  (let [n (math.sqrt n2)
        res (matrix n n 0)]
    (var v n2)
    (var xn n)
    (var yn n)
    (var x0 1)
    (var y0 1)
    (while (< 0 v)
      (for [j xn x0 -1]
        (table-replace res yn j v)
        (set v (- v 1)))
      (for [i (- yn 1) y0 -1]
        (table-replace res i y0 v)
        (set v (- v 1)))
      (for [j (+ 1 x0) xn 1]
        (table-replace res y0 j v)
        (set v (- v 1)))
      (for [i (+ y0 1) (- yn 1) 1]
        (table-replace res i xn v)
        (set v (- v 1)))
      (set yn (- yn 1))
      (set xn (- xn 1))
      (set y0 (+ y0 1))
      (set x0 (+ x0 1)))
    res))

(fn nearest-square [m]
  "finds smallest n² s.t. n² > m e.g. 24->25"
  (^ (+ 1 (math.floor (math.sqrt m))) 2))

(fn file-exists? [path]
  "returns true if path exists on fs"
  (not= nil (io.open path)))

(fn file-copy [from to files]
  "copies each file of files from directory tree from to directory tree to"
  (each [_ file (ipairs files)]
    (let [cmd (.. "cp -f " "'" from "/" file "' '" to "/" file "'")]
      (os.execute cmd))))

(fn int-len [x]
  "returns length of integer literal"
  (+ 1 (math.floor (math.log (+ (math.abs x) 0.1) 10))))

(fn int-halves [x]
  "splits integer literal into two halfs e.g. 1000->[10 0]"
  (let [len (int/ (int-len x) 2)]
    [(int/ x (^ 10 len))
     (% x (^ 10 len))]))

{: string-from
 : bytes-from
 : string-escape
 : string-last-index-of
 : string-indices
 : string-all-indices
 : string-pushback
 : string-tonumarray
 : string-toarray
 : string-ends-with
 : string-starts-with
 : string-split
 : string-trim
 : string-trim2 
 : string-shift
 : string-subs
 : array-to-number
 : math-sum
 : math-pow
 : pow
 : pown
 : math-gcd
 : math-lcm
 : math-fact
 : math-min
 : math-max
 : min-index
 : max-index
 : table-print
 : table-sum
 : table-prod
 : table-filter
 : table-count
 : table-sort
 : table-join
 : table-join3 
 : table-identical-2d?
 : table-identical?
 : table-equals
 : table-zip
 : table-unzip 
 : table-reverse
 : matrix-reverse
 : table-reset
 : table-remove 
 : table-contains?
 : table-zero?
 : table-range
 : table-apply
 : table-clone
 : table-group-by
 : table-transpose
 : table-column-sum
 : table-replace
 : table-replace-row
 : table-swap
 : table-index-swap
 : table-prod-kv
 : permutation
 : table-move
 : string-totable
 : table-tostring
 : matrix-print  
 : table-unique
 : head
 : tail
 : not-empty?
 : first
 : last
 : take
 : take-n
 : rest
 : fold
 : empty?
 : table-contains-2d?
 : matrix-contains?
 : range
 : range-to
 : range-step
 : range-of
 : tonum
 : todecimal
 : dec
 : inc
 : new-matrix
 : matrix
 : read-matrix
 : write-matrix
 : matrix-swap
 : matrix-set
 : matrix-toggle
 : matrix-apply
 : matrix-clone
 : matrix-adjvals
 : matrix-match-count
 : matrix-find
 : matrix-count
 : matrix-index
 : matrix-print
 : table-unpack
 : powerset
 : combination
 : max
 : min
 : table-disjunc 
 : table-tonumber
 : table-min
 : table-max
 : table-exclude
 : table-exclude-all
 : table-flatten
 : table-flatten2
 : table-union 
 : table-sum-if
 : runtime
 : int/
 : concat
 : tolong
 : toarray
 : lazy-seq
 : intersection
 : manhattan-dist
 : hamming-dist
 : decartian
 : in-segment?
 : partition-by
 : partition-identity
 : partition-at 
 : partition1
 : partition3step1
 : partition2
 : partition3
 : partitionm
 : table-no-dups?
 : frequency
 : rank
 : adjacency-list
 : keys
 : vals
 : adjacency-root
 : collide?
 : dist2rd
 : qpush
 : qpop
 : qpeek
 : push
 : pop
 : peek
 : ppush
 : ppop
 : odd?
 : even?
 : table-odd
 : table-even
 : table-update
 : table-upsert
 : table-mid
 : html
 : int
 : xor
 : modulo+
 : mod
 : turtle-new
 : turtle-pen-up?
 : turtle-pen-up
 : turtle-pen-down
 : turtle-pen-color
 : turtle-right
 : turtle-left
 : turtle-draw-line
 : turtle-forward
 : turtle-back
 : turtle-go
 : turtle-toward
 : turtle-distance
 : turtle-write
 : prob-bayes
 : prob-mean
 : prob-variation
 : prob-dist-binom
 : prob-dist-geom
 : prob-nchoosei
 : prob-binom
 : table-intersect
 : table-shift
 : table-index-loop
 : ulam-spiral
 : nearest-square
 : file-exists?
 : file-copy
 : int-len
 : int-halves
 }

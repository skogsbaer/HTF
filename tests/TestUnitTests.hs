import Test.Framework

data T = A | B deriving Eq

stringGap = "hello \
            \world!"

$(tests "assertTests" [d|

 testStringGap = assertEqual stringGap "hello world!"

 testAssertEqual = assertEqual 1 2

 testAssertEqual2 = assertEqual2 A B

 testAssertSeqEqual = assertSeqEqual [1,2] [2]

 testAssertSeqEqualSuccess = assertSeqEqual [1,2] [2,1]

 testAssertNotNull = assertNotNull []

 testAssertNull = assertNull [1]
 |])

allTests = TestList [assertTests]

main = runTestTT allTests
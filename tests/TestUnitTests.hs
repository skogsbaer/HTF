import Test.Framework

data T = A | B deriving Eq

stringGap = "hello \
            \world!"

$(tests "assertTests" [d|

 testStringGap = assertEqual stringGap "hello world!"

 testAssertEqual = assertEqual 1 2

 testAssertEqualNoShow = assertEqualNoShow A B

 testAssertSetEqual = assertSetEqual [1,2] [2]

 testAssertSetEqualSuccess = assertSetEqual [1,2] [2,1]

 testAssertNotNull = assertNotNull []

 testAssertNull = assertNull [1]

 testAssertThrows = assertThrows (return ()) (\_ -> True)

 testAssertThrows' = assertThrows (error "ERROR") (\_ -> False)
 |])

allTests = TestList [assertTests]

main = runTestTT allTests
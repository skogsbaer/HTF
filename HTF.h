#define SRC_LOC_ (makeLoc __FILE__ __LINE__)

#define assertBool (assertBool_ SRC_LOC_)
#define assertEqual (assertEqual_ SRC_LOC_)
#define assertEqualNoShow (assertEqualNoShow_ SRC_LOC_)
#define assertSetEqual (assertSetEqual_ SRC_LOC_)
#define assertEmpty (assertEmpty_ SRC_LOC_)
#define assertNotEmpty (assertNotEmpty_ SRC_LOC_)
#define assertThrows (assertThrows_ SRC_LOC_)

#include "gtest/gtest.h"

extern "C" {
    #include "doubly/doubly.h"
}

TEST(doubly, ok) {
    ASSERT_EQ(0, 0);
}

TEST(doubly, not_ok) {
    ASSERT_EQ(1, 0);
}

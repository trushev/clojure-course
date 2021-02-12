# Clojure Course

### Run Main
    $ lein run
    Hello, World!

### Run tests
    $ lein test
    
    lein test clojure-course.core-test
    
    lein test :only clojure-course.core-test/a-test
    
    FAIL in (a-test) (core_test.clj:7)
    FIXME, I fail.
    expected: (= 0 1)
    actual: (not (= 0 1))
    
    Ran 1 tests containing 1 assertions.
    1 failures, 0 errors.
    Tests failed.

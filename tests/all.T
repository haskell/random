test('random1283',
     [reqlib('containers'),expect_broken(2844)],
     compile_and_run,
     ['-package containers'])

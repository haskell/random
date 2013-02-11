
test('rangeTest',
     when(opsys('mingw32'), expect_broken(7379)),
     compile_and_run,
     [''])

test('random1283',
     reqlib('containers'),
     compile_and_run,
     [' -package containers'])

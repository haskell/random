# This lets me run tests from the test directory during development:
# opts = '-i.. -XCPP '

test('rangeTest',
     normal,
     compile_and_run,
     [opts])

test('random1283',
     reqlib('containers'),
     compile_and_run,
     [opts + ' -package containers'])

# Linear Vesting Tests Documentation

This offers a comprehensive guide to the Linear Vesting tests within the project. The property based tests (in `./LinearVestingSpec.hs`) are meticulously designed to assess the accuracy of Linear Vesting implementation, particularly focusing on the validation of actions involved during the unlocking phase of vesting.

## Overview

The property based tests for the Linear Vesting are organized into a suite titled "Linear Vesting Tests", This suite is dedicated to verifying the validation process for the accurate implementation of Linear Vesting Contracts.

## Test Suite Details

### Tests Included

-**Partial Unlock**: This property based test single-handedly confirms the proper initialization of Linear Vesting Contract and the successful unlocking of vested assets from the Contract at correct intervals. It does so by programmatically generating **100** test scenarios with different Vesting datum values per test run, unlock timings and their corresponding vested asset quantities. Since the whole test suit is ran automatically with each commit, this means that thousands (if not millions) of test cases are tried during the whole lifetime of the project. This confirms that the Contract allows accurate vesting of assets and their claim under varied conditions.

## Running the Tests

To execute the Linear Vesting Tests, you should follow the standard testing procedures outlined in the project documentation. Typically, this involves executing a command such as:

```sh
cabal new-test --test-show-details=streaming
```

This command will compile and execute all the test suites defined in the project. The output will show the status of each test case.

### Test Outcome Summary

In the most recent execution:

```markdown
Test suite linear-vesting-test: RUNNING...
linear-vesting
  Linear Vesting Tests
    Partial Unlock
      succeeds with correct parameters: OK (7.59s)
        +++ OK, passed 100 tests.
```

A history of test execution results can be found on Github at [Linear Vesting Tests](https://github.com/Anastasia-Labs/linear-vesting/actions)

### Execution Time

The entire suite was executed in 7.6 seconds, showcasing the Vesting validation process's speed and efficiency of taking just 0.07 seconds per test scenario.

## Conclusion

The property based tests for the Linear Vesting project plays a vital role in verifying the integrity, functionality, robustness and accuracy of the Linear Vesting Implementation.
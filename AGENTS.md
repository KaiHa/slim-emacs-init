# Slim Emacs configuration

## Information for AI Agents

### Rules
- Don't alter the control characters (e.g. '', '') found in `lisp/libkai.el` and other files

### Files in this project
- **lisp/libkai.el**: Contains the main power supply control functions and the shortcuts interface
- **tests.el**: Contains test cases for the power supply functions
- **manual-shortcuts-test.el**: Script for manual testing of the shortcuts interface

### Testing Information

- Run tests with: `./run-tests.sh`
- All tests should pass (8 tests total)
- The test suite includes verification of:
  - ADP device detection
  - Manson power supply functions
  - AIM-TTi power supply functions
  - Custom confirmation function behavior
  - Buffer manipulation utilities

### Development Notes

- The codebase uses `when-let` for conditional execution with local bindings
- Power supply functions follow a consistent naming pattern: `kai/{type}-{action}`
- Device detection functions use `kai/find-serial-devices` with vendor-specific patterns
- The shortcuts interface uses org-mode links with elisp protocol for interactive execution

### Useful Patterns

1. **Device Detection Pattern**:
```elisp
(defun kai/{type}-get-devs ()
  "Find all {type} devices."
  (kai/find-serial-devices "Vendor Specific Pattern"))
```

2. **Device Control Pattern**:
```elisp
(defun kai/{type}-{action} (tty)
  "Perform {action} on {type} device at TTY."
  (interactive (list (kai/read-tty-path (kai/{type}-get-devs))))
  (kai/{type}-instruct tty "COMMAND"))
```

3. **Shortcuts Integration Pattern**:
```elisp
(when-let ((devs (kai/{type}-get-devs)))
  (insert "* {Type} Power Supply\n\n")
  (dolist (tty devs)
    (insert (format "[[elisp:(kai/{type}-{action} \"%s\")][{Action}]]" tty))))
```

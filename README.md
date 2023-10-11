# OpenAPI Mode #

Minor mode for OpenAPI & Swagger yaml files. Provides support jumping
to definitions & references using `xref`. Navigation helpers to narrow
to block and move across yaml blocks.

## Keybindings ##

Standard `xref` keybindings for jump to definition <kbd>M-.</kbd>,
jump back <kbd>M-,</kbd> & show references <kbd>M-?</kbd> are used.

| Feature                         | Keybinding           | OpenAPI            | Swagger              |
|---------------------------------|:--------------------:|:------------------:|:--------------------:|
| Move forward yaml block         | <kbd>C-M-f</kbd>     | :white_check_mark: | :white_check_mark:   |
| Move backward yaml block        | <kbd>C-M-b</kbd>     | :white_check_mark: | :white_check_mark:   |
| Copy yaml block                 | <kbd>C-c C-w</kbd>   | :white_check_mark: | :white_check_mark:   |
| Cut yaml block                  | <kbd>C-c C-k</kbd>   | :white_check_mark: | :white_check_mark:   |
| Select/highlight yaml block     | <kbd>C-c C-SPC</kbd> | :white_check_mark: | :white_check_mark:   |
| Narrow to yaml block            | <kbd>C-x n b</kbd>   | :white_check_mark: | :white_check_mark:   |
| Jump to path operation          | <kbd>C-c C-p</kbd>   | :white_check_mark: | :white_check_mark:   |
| Jump to section                 | <kbd>C-c C-s</kbd>   | :white_check_mark: | :white_large_square: |
| Insert component/definition ref | <kbd>C-c C-c</kbd>   | :white_check_mark: | :white_check_mark:   |
| Insert parameter ref            | <kbd>C-c i p</kbd>   | :white_check_mark: | :white_large_square: |
| Insert header ref               | <kbd>C-c i p</kbd>   | :white_check_mark: | :white_large_square: |
| Insert response ref             | <kbd>C-c i r</kbd>   | :white_check_mark: | :white_large_square: |
| Insert request body ref         | <kbd>C-c i b</kbd>   | :white_check_mark: | :white_large_square: |
| Insert scheam ref               | <kbd>C-c i s</kbd>   | :white_check_mark: | :white_large_square: |


**NOTE**: Moving back a block moves to the last yaml block moved forward

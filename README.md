# discrete-output-machine

*discrete-output-machine* is an abstraction layer to make creation of terminal 
applications easy.  

Terminal screen area is represented with a buffer, which is a table of cells.
Redrawing is performed manually, on `buffer-redraw` call.
A cell is a pair of 2D coordinates and a colored character. When colors or
character is changed, the cell is redrawn; when coordinates are changed,
the cell is moved.

This project utilizes `cl-multiagent-system:define-entity` macro to implement
all public interface facilities. The code is self-explanatory;
the interested reader is invited to get acquainted with the implementation 
themselves.

Modules of the project:

* terminal IO functions (`terminal-io.lisp`);

* coordinates, colored character and cell (coordinates + colored character)
entities (`cell.lisp`);

* buffer of cells (`buffer.lisp`).

## Author

Ivan Podmazov (ivanpzv8@gmail.com)

## [License](LICENSE)

Copyright (c) 2019 Ivan Podmazov


# discrete-output-machine

*discrete-output-machine* is an abstraction layer to make creation of terminal 
applications easy.  
Changes of properties are reflected on the screen on-the-fly.

## API

### Manual IO

#### format/ansi-sequence (stream control-string *&rest* args)

Write string beginning with an ANSI escape code to `stream`.

#### hide-cursor (stream)

#### show-cursor (stream)

#### enable-alternative-screen (stream)

#### disable-alternative-screen (stream)

#### clear-screen (stream)

#### go-to (stream x y)

Move cursor to (x, y).

#### set-color (stream fg bg)

Write next characters using foreground color `fg` and background color `bg` 
in 256 color palette.

#### put-character (stream x y fg bg chr)

Move cursor, set foreground and background colors and write a single character.

### Cell

#### \*default-cell-x\*

Default x coordinate for a freshly created cell.

#### \*default-cell-y\*

Default y coordinate for a freshly created cell.

#### \*default-cell-chr\*

Default character for a freshly created cell.

#### \*default-cell-fg\*

Default foreground color (in 256 color palette) for a freshly created cell.

#### \*default-cell-bg\*

Default background color (in 256 color palette) for a freshly created cell.

#### \*default-cell-visibility\*

Default visibility state for a freshly created cell.

#### \*default-cell-buffer\*

Default buffer to attach to for a freshly created cell.

#### make-cell (id *&key* metadata x y chr fg bg visibility buffer)

Create a cell with `id` and the specified properties.

#### cell-id (cell)

Cell ID value.

#### *SETFABLE* cell-data (cell)

Cell data.

#### *SETFABLE* cell-x (cell)

Cell x coordinate.

#### *SETFABLE* cell-y (cell)

Cell y coordinate.

#### *SETFABLE* cell-chr (cell)

Cell character.

#### *SETFABLE* cell-fg (cell)

Cell foreground color in 256 color palette.

#### *SETFABLE* cell-bg (cell)

Cell background color in 256 color palette.

#### *SETFABLE* cell-visibility (cell)

Cell visibility (generalized boolean).

#### *SETFABLE* cell-buffer (cell)

Cell parent buffer or NIL (if cell is not attached to any buffer).
Cell parent buffer cannot be changed to another instantly, the following
steps must be made:

1. Unregister cell from an old buffer:

`(setf (cell-buffer cell) nil)`

2. Wait until an old cell parent buffer is redrawn (or redraw manually):

`(buffer-redraw old-buffer)`

3. Register cell to a new buffer:

`(setf (cell-buffer cell) new-buffer)`

#### cell-acquire-lock (cell)

Acquire cell's lock.

#### cell-release-lock (cell)

Release cell's lock.

#### with-cell-lock-held ((cell) &body body)

Execute code with cell's lock held.

### Multicell (rectangular group of cells)

#### \*default-multicell-data-fn\*

Default child cell data generator function.

#### \*default-multicell-size-x\*

Default multicell width.

#### \*default-multicell-size-y\*

Default multicell height.

#### \*default-multicell-x\*

Default multicell top-left corner x coordinate.

#### \*default-multicell-y\*

Default multicell top-left corner y coordinate.

#### \*default-multicell-visibility\*

Default multicell visibility.

#### make-multicell (id *&key* data-fn size-x size-y x y visibility buffer)

Create a multicell with the specified properties.

#### multicell-id (multicell)

Multicell ID value.

#### multicell-size-x (multicell)

Multicell x size.

#### multicell-size-y (multicell)

Multicell y size.

#### *SETFABLE* multicell-x (multicell)

Multicell top-left x coordinate.

#### *SETFABLE* multicell-y (multicell)

Multicell top-left y coordinate.

#### *SETFABLE* multicell-visibility (multicell)

Multicell visibility.

#### *SETFABLE* multicell-buffer (multicell)

Multicell buffer.

#### *SETFABLE* multicell-data (multicell x y)

Multicell's child cell data.

#### *SETFABLE* multicell-chr (multicell x y)

Multicell's child cell character.

#### *SETFABLE* multicell-fg (multicell x y)

Multicell's child cell foreground color in 256 color palette.

#### *SETFABLE* multicell-bg (multicell x y)

Multicell's child cell background color in 256 color palette.

#### multicell-acquire-lock (multicell)

Acquire multicell's lock.

#### multicell-release-lock (multicell)

Release multicell's lock.

#### with-multicell-lock-held ((multicell) &body body)

Execute code with multicell's lock held.

#### multicell-acquire-cells-locks (multicell)

Acquire locks of all cells of a multicell.

#### multicell-release-cells-locks (multicell)

Release locks of all cells of a multicell.

#### with-multicell-cells-locks-held ((multicell) &body body)

Execute code with all multicell child cells' locks held.

#### 

### Buffer

#### \*default-buffer-stream\*

Default stream to be assigned to for a freshly created buffer.

#### \*default-buffer-cell-priority-fn\*

Default cell priority predicate function for a freshly created buffer.

#### \*default-buffer-blank-fn\*

Default blank space visualization function for a freshly created buffer.

#### \*default-buffer-displ-x\*

Default horizontal output area displacement for a freshly created buffer.

#### \*default-buffer-displ-y\*

Default vertical output area displacement for a freshly created buffer.

#### \*default-buffer-size-x\*

Default horizontal output area size for a freshly created buffer.

#### \*default-buffer-size-y\*

Default vertical output area size for a freshly created buffer.

#### make-buffer (*&key* stream cell-priority-fn blank-fn displ-x displ-y size-x size-y)

Create a buffer with the specified properties.

#### buffer-stream (buffer)

Buffer output stream.

#### *SETFABLE* buffer-cell-priority-fn (buffer)

Buffer cell priority predicate function (type `(cell1 cell2) => boolean`,
`NIL` if `cell1` is upper than `cell2`, `T` otherwise; `cell1` always
occupied position more recently than `cell2`).

#### *SETFABLE* buffer-blank-fn (buffer)

Buffer blank space visualization function (type `(x y) => chr fg bg`,
3 values are expected to be returned).

#### *SETFABLE* buffer-displ-x (buffer)

Buffer horizontal output area displacement.

#### *SETFABLE* buffer-displ-y (buffer)

Buffer vertical output area displacement.

#### *SETFABLE* buffer-size-x (buffer)

Buffer horizontal output area size.

#### *SETFABLE* buffer-size-y (buffer)

Buffer vertical output area size.

#### buffer-cells (buffer)

List of all registered cells of a buffer.

#### buffer-refresh (buffer)

Request full area redraw at next `buffer-redraw`.

#### buffer-redraw (buffer)

Update and redraw any changed cells.

#### buffer-acquire-lock (buffer)

Acquire buffer's lock.

#### buffer-release-lock (buffer)

Release buffer's lock.

#### with-buffer-lock-held ((buffer) &body body)

Execute code with buffer's lock held.

## Author

Ivan Podmazov (ivanpzv8@gmail.com)

## [License](LICENSE)

Copyright (c) 2019 Ivan Podmazov


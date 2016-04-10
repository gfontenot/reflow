# reflow

Intelligently reflow text to a given length.

`reflow` is like `fold(1)` on steroids:

 - Break lines on word boundaries by default (mimicking `fold -s`)
 - Doesn't reflow markdown code blocks
 - Doesn't reflow markdown/email quote blocks
 - Optionally doesn't reflow email headers

## Usage

```
Usage: reflow [-w|--width WIDTH] [--ignore-headers] [PATH]
  Intelligently reflow text to a given length

Available options:
  -h,--help                Show this help text
  -w,--width WIDTH         Specify a line width to use instead of the default 80
                           columns
  --ignore-headers         Don't wrap email headers
  PATH                     Path to input file. If not provided, reflow will
                           attempt to read from STDIN
```

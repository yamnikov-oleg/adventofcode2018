```
$ racket day10.rkt example.txt
```

This solution doesn't implement text recognition, because I'm lazy. Instead, it prints the picture made by points onto the screen letting user recognize the text and press Ctrl+C to stop the simulation.

Since all the points are initially wide-spread, picture can't be printed every second (it's too big). So the picture is only printed when its size doesn't excede 100 points by width and height.

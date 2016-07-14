Hey everyone,

This is a fun little function I made to shuffle rows in a data frame in the way I'd like to see songs shuffled in a playlist (let's just say that I'm not super thrilled with how Spotify's shuffle algorithm works).

This shuffle algorithm will...

* randomly select one row at a time from the data set, favoring rows that have been selected less than other rows
* exclude rows from the pool of selectable rows for a certain number of "plays" (user defined) after they have been selected, and
* never select two rows in order

This is just a function I made when I was bored for fun, feel free to copy and paste the code into R and try it out for yourself on the `mtcars` data set or something similar. You'll get the gist of it pretty quickly.

Email n_martinez3@u.pacific.edu for any questions/comments/criticisms/concerns/etc.

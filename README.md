# batwikitwitter

Update: due to some changes made to wikipedia itself this bot now crashes a lot, and usually fails to pull wikipedia photos. I'll rewrite it at some point.

Before using the script to use a twitter bot, you'll need to create a token for your bot using twitters apps. Instructions can be found here http://rtweet.info/articles/auth.html though note I didn't bother making the token into a environment variable, I found it easier just to load mine in at the beginning of this script. Whatever works best for you


The script allows you to use it for scheduling, in which case you will just leave this r session running indefinitely on your machine. This isn't exactly ideal, and there are various scheduling programmes available (e.g. crontab) available for different operating systems. If you want to use one of those, just set wait_in_r to FALSE

This bot starts by querying your given list of pages, selecting a given page within there. If it is able to find one that passes various basic QC measures, it looks on wikimedia for the page's main image, then gets the image creator's creditation. If all of these checks are passed, it downloads the picture and then tweets your message and image.

Then, depending on what wait_in_r is been set to, the script either ends or waits for the next iteration.

This code is free for you to use and modify as you see fit. All I ask in return is that in your bots bio you credit me with 'bot made with code by @hammerheadbat'

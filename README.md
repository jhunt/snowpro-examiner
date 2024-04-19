# Snowflake SnowPro Practice Test

It's a practice test, network-accessible, with generated questions
based on a factual assertion database.

To run it locally:

```console
$ ./run
```

That will spin up a docker container on TCP/2177 that you can
telnet to for the fully-interactive test-taking experience:

```console
$ telnet localhost 2177
Trying ::1...
Connected to localhost.localdomain.
Escape character is '^]'.

     \__  __/
     /_/  \_\        Snowflake SnowPro Quiz ❄️
      _\/\/_
 __/\_\_\/_/_/\__    Think you know Snowflake?
   \/ /_/\_\ \/
     __/\/\__        Answer T/F questions by typing T or F.
     \_\  /_/        Answer multiple choice with single letter
     /      \        choices wrapped in parentheses, i.e. (A C D)

                     made with ❤️ by vivanti.


163 facts known to the examiner

1) The OWNERSHIP privilege on contained objects in the database can be granted to a database role.

T or F? T

2) Snowflake automatically extends the data retention period for streams on tables with less than 14 days of time travel

T or F? F
Incorrect!
Correct answer was (T)
Final Score: 1

Go again? (Y/N)
```

No scores or responses are saved in any way, shape, or form.  If
you want to remember something, you'd better write it down
yourself.

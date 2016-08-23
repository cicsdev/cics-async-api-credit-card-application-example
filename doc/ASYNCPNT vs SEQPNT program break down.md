# `ASYNCPNT` vs `SEQPNT` program break down

This example is designed to simulate a real-world scenario: a customer applies for a credit card, and an automated program kicks off to check their name, details, and history as part of the application process.

Our two programs `ASYNCPNT` and `SEQPNT` use the same programs, but run them in a different order.

`SEQPNT`:

![SEQPNT diagram][seqdiagram]

`ASYNCPNT`:

![ASYNCPNT diagram][asyncdiagram]

`SEQPNT` calls the involved programs using `EXEC CICS LINK`, and `ASYNCPNT` uses the new `EXEC CICS RUN TRANSID` command. The child programs take the same amount of time to complete in both `ASYNCPNT` and `SEQPNT`, but `ASYNCPNT` finishes in roughly half the time thanks to the asynchronous API.

###### Sequential processing:

![Sequential processing][seqflow]

###### Asynchronous processing:

![Asynchronous processing][asyncflow]



[asyncdiagram]: https://github.com/cicsdev/cics-async-api-credit-card-application-example/blob/master/doc/credit-card-application-diagram-async.png
[seqdiagram]: https://github.com/cicsdev/cics-async-api-credit-card-application-example/blob/master/doc/credit-card-application-diagram-seq.png
[seqflow]: https://github.com/cicsdev/cics-async-api-credit-card-application-example/blob/master/doc/seq.gif
[asyncflow]: https://github.com/cicsdev/cics-async-api-credit-card-application-example/blob/master/doc/async.gif

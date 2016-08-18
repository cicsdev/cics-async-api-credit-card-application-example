# CICS Asynchronous API Credit Card Application Example

This example has two parent programs: `SEQPNT` and `ASYNCPNT`. `SEQPNT` runs our
credit check application in a traditional sequential manner, while `ASYNCPNT`
runs the same application using the new CICS Asynchronous API commands.

Both programs can be run from a CICS terminal screen or from a web
service. Using the terminal screen requires less set up, and you can see the
progress of the application over time, but running from a web service exposes
more of the data being passed between parent and child tasks via CICS
channels. This readme covers the terminal screen invocation.

Note: Also included in the source are CICS bundles with all the definitions you
need to get up and running quickly if you're comfortable with bundles.

## Set Up

1. Download the source code from this GitHub repository as a .zip file and
   extract to your preferred location.
2. Compile the COBOL source code in the _src_ directory.
3. Define the transactions and programs in your CSD, and add the load library to
   the RPL (or define a library resource in the CSD).

Here are some CSD definitions we made earlier:

    DEFINE PROGRAM(ASYNCPNT) GROUP(AS) STATUS(ENABLED) DEFINE
    TRANSACTION(ACCA) GROUP(AS) PROGRAM(ASYNCPNT)
    
    DEFINE PROGRAM(CRDTCHK) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(ICCK) GROUP(AS) PROGRAM(CRDTCHK)
    
    DEFINE PROGRAM(CSSTATS2) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(STUS) GROUP(AS) PROGRAM(CSSTATS2)
    
    DEFINE PROGRAM(CSSTATS) GROUP(AS) STATUS(ENABLED)
    
    DEFINE PROGRAM(GETADDR) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(GETA) GROUP(AS) PROGRAM(GETADDR)
    
    DEFINE PROGRAM(GETNAME) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(GETN) GROUP(AS) PROGRAM(GETNAME)
    
    DEFINE PROGRAM(GETPOL) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(GETP) GROUP(AS) PROGRAM(GETPOL)
    
    DEFINE PROGRAM(GETSPND) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(SPND) GROUP(AS) PROGRAM(GETSPND)
    
    DEFINE PROGRAM(SEQPNT) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(SCCA) GROUP(AS) PROGRAM(SEQPNT)
    
    DEFINE PROGRAM(UPDCSDB) GROUP(AS) STATUS(ENABLED)
    DEFINE TRANSACTION(UPDB) GROUP(AS) PROGRAM(UPDCSDB)
 
## Running the Example

### Using a CICS Terminal

At the terminal screen, enter the transaction you wish to run, followed by a
four-digit customer ID. Our sequential application is transaction `SCCA`, and
our asynchronous application is transaction `ACCA`.

The example application will recognise any four-digit customer ID. For example:

    SCCA 0002

or

    ACCA 0001

The screen may seem to hang, but that's just the time it takes for the request
to complete. On average, it's around 18 seconds for the sequential application,
and 9 seconds for the asynchronous version.

## License

This project is licensed under [Apache License Version 2.0](LICENSE).

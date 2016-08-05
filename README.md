# CICS Asynchronous API Credit Card Application Example

This example has two parent programs: `SEQPNT` and `ASYNCPNT`. `SEQPNT` runs our
credit check application in a traditional sequential manner, while `ASYNCPNT`
runs the same application using the new CICS Asynchronous API commands.

Both programs can be run from either a CICS terminal screen or a web
service. Using the terminal screen requires less set up, and you can see the
progress of the application over time, but running from a web service exposes
more of the data being passed between parent and child tasks via CICS channels.

## Set Up

1. Download the source code from this Github repository as a .zip file and
   extract to your preferred location.
2. Compile the COBOL source code into a dataset. (If you're going to run from a
   terminal, you can ignore the WSDL and WSBind files.)
3. Using CICS Explorer, import the CICS bundles
   (*cics-async-credictcardapplication-pipeline*,
   *cics-async-creditcardapplication-resources*, and
   *cics-async-creditcardapplication-webservices*) as an existing project into
   the Project Explorer view.
4. In the *cics-async-creditcardapplication-resources* bundle, edit the library
   resource (ASYNCLIB) and replace the dataset placeholder with the location
   where you compiled your COBOL in step 2.
5. Save the *cics-async-creditcardapplication-resources* bundle and deploy
   (export and install bundle) to your CICS region.

If you're running from the CICS terminal, that's all the set up you
need. Continue to the *Running the example* section.

Web service execution steps:

1. In the *cics-async-creditcardapplication-pipeline* bundle, edit the TCP/IP
   service (HTTPWS) to provide a suitable port number that can be used to direct
   incoming HTTP web service requests.
2. Deploy the *cics-async-creditcardapplication-pipeline* bundle to your CICS
   region.
3. Deploy the *cics-async-creditcardapplication-webservices* bundle to your CICS
   region. (No edits required.)

Note: If you don't want to use CICS bundles, it's possible to manually define
the 9 transactions, 10 programs, and the library, or create some CSDUP JCL.
 
## Running the Example

### Using a CICS Terminal

At the terminal screen, enter the transaction you wish to run, followed by a
four-digit customer ID. Our sequential application is transaction `SCCA`, and
our asynchronous application is transaction `ACCA`.

The example application will recognise any four-digit customer ID. For example:
```
SCCA 0002
```
or
```
ACCA 0001
```

### Using a Web Service

You'll need a utility to run web service requests, such as the *Test with Web
Service Explorer* in RDz. That's what we'll use in this explanation.

In RDz, right-click on the WSDL file you want to use, and select *Test with Web
Service Explorer*. Use *SEQPNT.wsdl* for the sequential application, and
*AYNCPNT.wsdl* for the asynchronous application.
 
Update the URI with the CICS host name and port number you provided in step 1 of
the *Web service execution steps*. Enter a customer ID number (0001, for
example) in the web service form, then you can run the service.

The screen may seem to hang, but that's just the time it takes for the request
to complete. On average, it's around 18 seconds for the sequential application,
and 9 seconds for the asynchronous version.


## License

This project is licensed under [Apache License Version 2.0](LICENSE).

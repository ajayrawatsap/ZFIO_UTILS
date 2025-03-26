# ZFIO_UTILS
Handle Operations Related to Files in ABAP Cloud

In ABAP BTP cloud environment with eclipse there is no known way of downloading a file form a ABAP program
to local PC.
The on premise solutions like using  cl_gui_frontend_services do not work in ABAP cloud with Eclipse ADT.
To overcome this issue this ABAP repo build the functionality in two steps:

Step:1 Save the file contents as RAWSTRING in database table zfio_files
Step2: use table zfio_files to generate a ABAP RAP based Fiori elements UI.

The Fiori applciation streams the contents of file on UI as attachment from where user can downlaod the file.

In the ABAP BTP cloud environment with Eclipse, there is no known way of downloading a file from an ABAP program to a local PC. The on-premise solutions like using cl_gui_frontend_services do not work in ABAP cloud with Eclipse ADT. This ABAP repository builds the functionality in two steps:

Step 1
Save the file contents as RAWSTRING in database table zfio_files

Step 2
Use table zfio_files to generate an ABAP RAP based Fiori elements UI

The Fiori application streams the contents of the file on the UI as an attachment from where users can download the file.

Implementation Details
Database Storage: Files are stored in the zfio_files table as RAWSTRING
UI Layer: ABAP RAP (RESTful Application Programming) model is used to expose the data
User Experience: Files appear as attachments in the Fiori UI for easy download
Usage
To use this functionality:

Store your file content using the provided ABAP class
Access the Fiori application to view and download the files

# ABAP BTP Cloud File Download Solution

In the ABAP BTP cloud environment with Eclipse, there is no known way of downloading a file from an ABAP program to a local PC. The on-premise solutions like using `cl_gui_frontend_services` do not work in ABAP cloud with Eclipse ADT. This ABAP repository builds the functionality in two steps:

## Step 1
Save the file contents as `RAWSTRING` in database table `zfio_files`.

## Step 2
Use table `zfio_files` to generate an ABAP RAP based Fiori elements UI.

The Fiori application streams the contents of the file on the UI as an attachment from where users can download the file.

## Implementation Details
- **Database Storage**: Files are stored in the `zfio_files` table as `RAWSTRING`.
- **UI Layer**: ABAP RAP (RESTful Application Programming) model is used to expose the data.
- **User Experience**: Files appear as attachments in the Fiori UI for easy download.

## Usage
To use this functionality:
1. Store your file content using the provided ABAP class.
2. Access the Fiori application to view and download the files.

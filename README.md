# ZFIO_UTILS
Handle Operations Related to Files in ABAP Cloud

In ABAP BTP cloud environment with eclipse there is no known way of downloading a file form a ABAP program
to local PC.
The on premise solutions like using  cl_gui_frontend_services do not work in ABAP cloud with Eclipse ADT.
To overcome this issue this ABAP repo build the functionality in two steps:

Step:1 Save the file contents as RAWSTRING in database table zfio_files
Step2: use table zfio_files to generate a ABAP RAP based Fiori elements UI.

The Fiori applciation streams the contents of file on UI as attachment from where user can downlaod the file.

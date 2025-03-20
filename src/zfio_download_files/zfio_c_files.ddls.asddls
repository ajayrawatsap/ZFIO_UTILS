@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Projection View for Files'

@Metadata.ignorePropagatedAnnotations: true

@Metadata.allowExtensions: true
define root view entity ZFIO_C_FILES
 provider contract transactional_query

  as projection on ZFIO_R_FILES as Files

{
  key FileUuid,

      FileName,
      
       @Semantics.largeObject: {
             acceptableMimeTypes: [ 'image/*', 'application/*', 'text/csv' ],
             cacheControl.maxAge: #MEDIUM,
             contentDispositionPreference:  #ATTACHMENT  , // #ATTACHMENT - download as file
                                                        // #INLINE - open in new window
             fileName: 'FileName',
             mimeType: 'FileMimetype'
           }
      FileContent,
      
       @Semantics.mimeType: true
      FileMimetype,
      
      LocalCreatedBy,
      
      LocalCreatedAt
}

@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View for Files'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZFIO_R_FILES as select from zfio_files
{
    key file_uuid as FileUuid,
    file_name as FileName,
    file_content as FileContent,
    file_mimetype as FileMimetype,
    local_created_by as LocalCreatedBy,
    local_created_at as LocalCreatedAt
}

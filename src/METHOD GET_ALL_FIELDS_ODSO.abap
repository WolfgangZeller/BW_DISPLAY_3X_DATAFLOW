* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_ODSO
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_ODSO                         TYPE        RSDODSOBJECT
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ALL_FIELDS_ODSO.

    DATA LT_ODSO TYPE STANDARD TABLE OF _TY_IOBJ_DETAILS WITH EMPTY KEY.
    DATA O_IOBJ_DETAILS TYPE REF TO CL_RSD_IOBJ.
    DATA LS_IOBJ_DETAILS TYPE RSD_S_VIOBJ.
    DATA lv_no type i value 1.

    SELECT IOBJNM
     FROM RSDODSOIOBJ
     WHERE ODSOBJECT = @I_ODSO
     AND OBJVERS = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @LT_ODSO.

    LOOP AT LT_ODSO ASSIGNING FIELD-SYMBOL(<FS_ODSO>).
      O_IOBJ_DETAILS = CL_RSD_IOBJ=>FACTORY( <FS_ODSO>-IOBJNM ).
      O_IOBJ_DETAILS->GET_INFO( EXPORTING I_OBJVERS = 'A' IMPORTING E_S_VIOBJ = LS_IOBJ_DETAILS ).
      <fs_odso>-no = lv_no.
      <FS_ODSO>-FIELDNM = LS_IOBJ_DETAILS-FIELDNM.
      <FS_ODSO>-IOBJTP = LS_IOBJ_DETAILS-IOBJTP.
      <FS_ODSO>-TXTSH = LS_IOBJ_DETAILS-TXTSH.
      <FS_ODSO>-TXTLG = LS_IOBJ_DETAILS-TXTLG.
      <FS_ODSO>-DATATP = LS_IOBJ_DETAILS-DATATP.
      <FS_ODSO>-LENGTH = LS_IOBJ_DETAILS-OUTPUTLEN.
      <FS_ODSO>-DECIMALS = LS_IOBJ_DETAILS-KYFDECIM.
      <FS_ODSO>-MD_ATTR = LS_IOBJ_DETAILS-ATTRIBFL.
      <FS_ODSO>-MD_TEXT  = SWITCH #( LS_IOBJ_DETAILS-TXTTABFL WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <FS_ODSO>-MD_HIER = LS_IOBJ_DETAILS-HIETABFL.
      lv_no = lv_no + 1.
    ENDLOOP.

    R_TABLE = LT_ODSO.

  ENDMETHOD.

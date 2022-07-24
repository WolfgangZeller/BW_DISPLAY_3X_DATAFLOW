* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_IOBJ
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_IOBJ                         TYPE        RSINFOCUBE
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ALL_FIELDS_IOBJ.

    DATA LT_IOBJ TYPE STANDARD TABLE OF _TY_IOBJ_DETAILS WITH EMPTY KEY.
    DATA O_IOBJ_DETAILS TYPE REF TO CL_RSD_IOBJ.
    DATA LS_IOBJ_DETAILS TYPE RSD_S_VIOBJ.
    DATA LV_NO TYPE I VALUE 1.

    SELECT ATTRINM as IOBJNM
     FROM RSDBCHATR
     WHERE CHABASNM = @I_IOBJ
     AND OBJVERS = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @LT_IOBJ.

    LOOP AT LT_IOBJ ASSIGNING FIELD-SYMBOL(<FS_IOBJ>).
      O_IOBJ_DETAILS = CL_RSD_IOBJ=>FACTORY( <FS_IOBJ>-IOBJNM ).
      O_IOBJ_DETAILS->GET_INFO( EXPORTING I_OBJVERS = 'A' IMPORTING E_S_VIOBJ = LS_IOBJ_DETAILS ).
      <FS_IOBJ>-NO = LV_NO.
      <FS_IOBJ>-FIELDNM = LS_IOBJ_DETAILS-FIELDNM.
      <FS_IOBJ>-IOBJTP = LS_IOBJ_DETAILS-IOBJTP.
      <FS_IOBJ>-TXTSH = LS_IOBJ_DETAILS-TXTSH.
      <FS_IOBJ>-TXTLG = LS_IOBJ_DETAILS-TXTLG.
      <FS_IOBJ>-DATATP = LS_IOBJ_DETAILS-DATATP.
      <FS_IOBJ>-LENGTH = LS_IOBJ_DETAILS-OUTPUTLEN.
      <FS_IOBJ>-DECIMALS = LS_IOBJ_DETAILS-KYFDECIM.
      <FS_IOBJ>-MD_ATTR = LS_IOBJ_DETAILS-ATTRIBFL.
      <FS_IOBJ>-MD_TEXT  = SWITCH #( LS_IOBJ_DETAILS-TXTTABFL WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <FS_IOBJ>-MD_HIER = LS_IOBJ_DETAILS-HIETABFL.
      LV_NO = LV_NO + 1.
    ENDLOOP.

    R_TABLE = LT_IOBJ.

  ENDMETHOD.

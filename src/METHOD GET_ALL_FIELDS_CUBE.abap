* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_3X_DATAFLOW=>GET_ALL_FIELDS_CUBE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CUBE                         TYPE        RSINFOCUBE
* | [<-()] R_TABLE                        TYPE        TT_IOBJ_DETAILS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ALL_FIELDS_CUBE.

    DATA LT_CUBE TYPE STANDARD TABLE OF _TY_IOBJ_DETAILS WITH EMPTY KEY.
    DATA O_IOBJ_DETAILS TYPE REF TO CL_RSD_IOBJ.
    DATA LS_IOBJ_DETAILS TYPE RSD_S_VIOBJ.
    DATA LV_NO TYPE I VALUE 1.

    SELECT IOBJNM
     FROM RSDCUBEIOBJ
     WHERE INFOCUBE = @I_CUBE
     AND OBJVERS = 'A'
     INTO CORRESPONDING FIELDS OF TABLE @LT_CUBE.
    DELETE LT_CUBE WHERE IOBJNM CS '__'.

    LOOP AT LT_CUBE ASSIGNING FIELD-SYMBOL(<FS_CUBE>).
      O_IOBJ_DETAILS = CL_RSD_IOBJ=>FACTORY( <FS_CUBE>-IOBJNM ).
      O_IOBJ_DETAILS->GET_INFO( EXPORTING I_OBJVERS = 'A' IMPORTING E_S_VIOBJ = LS_IOBJ_DETAILS ).
      <FS_CUBE>-NO = LV_NO.
      <FS_CUBE>-FIELDNM = LS_IOBJ_DETAILS-FIELDNM.
      <FS_CUBE>-IOBJTP = LS_IOBJ_DETAILS-IOBJTP.
      <FS_CUBE>-TXTSH = LS_IOBJ_DETAILS-TXTSH.
      <FS_CUBE>-TXTLG = LS_IOBJ_DETAILS-TXTLG.
      <FS_CUBE>-DATATP = LS_IOBJ_DETAILS-DATATP.
      <FS_CUBE>-LENGTH = LS_IOBJ_DETAILS-OUTPUTLEN.
      <FS_CUBE>-DECIMALS = LS_IOBJ_DETAILS-KYFDECIM.
      <FS_CUBE>-MD_ATTR = LS_IOBJ_DETAILS-ATTRIBFL.
      <FS_CUBE>-MD_TEXT  = SWITCH #( LS_IOBJ_DETAILS-TXTTABFL WHEN '1' THEN 'X'
                                                                 WHEN '0' THEN '' ).
      <FS_CUBE>-MD_HIER = LS_IOBJ_DETAILS-HIETABFL.
      LV_NO = LV_NO + 1.
    ENDLOOP.

    R_TABLE = LT_CUBE.

  ENDMETHOD.

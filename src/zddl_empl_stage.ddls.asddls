/*******************************************************************************

    DDL Source:     ZDDL_EMPL_STAGE
    View:           ZVW_EMPL_STAGE
    Author:         Erwin Wee
    Date:           12 February 2020
    
    Project:        Alliance Group Limited Infor HCM/ SAP Payroll Integration
    Transport:      xxxxxx

    Purpose:        Consolidate view of HR info types before postin
                    
    Notes:          
     
                        
********************************************************************************
                        M O D I F I C A T I O N S
********************************************************************************
*&-----------------------------------------------------------------------------*
*   DATE            MODIFIED BY         TRANSPORT           MODIFICATION KEY
*&-----------------------------------------------------------------------------*
*   12/02/2020      ERWIN WEE           XXXXX               MODIF01
*&-----------------------------------------------------------------------------*
*&  MODIF01 - Initial development
*&-----------------------------------------------------------------------------*

********************************************************************************/

@AbapCatalog.sqlViewName: 'ZVW_EMPL_STAGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Employee Master Staging view'
define view ZDDL_EMPL_STAGE 
as select from zdt_empl_msg as msg
inner join zdt_infty0000 as i0000
on msg.mssgguid = i0000.mssgguid
and msg.pernr = i0000.pernr
and msg.infotyp = i0000.infotyp
left outer join zdt_infty0002 as i0002
on msg.mssgguid = i0002.mssgguid
and msg.pernr = i0002.pernr
and msg.infotyp = i0002.infotyp
left outer join zdt_infty0001 as i0001
on msg.mssgguid = i0001.mssgguid
and msg.pernr = i0001.pernr
and msg.infotyp = i0001.infotyp
{
    msg.client,
    @EndUserText.label: 'Message GUID'
    msg.mssgguid as mssgguid,
    @EndUserText.label: 'Personnel number'
    msg.pernr as pernr,
    @EndUserText.label: 'InfoType'
    msg.infotyp as infotyp,
    @EndUserText.label: 'Message Timestamp'
    msg.msgtstamp as msgtstamp,
    @EndUserText.label: 'Processing Status'
    msg.procstat as procstat,
    @EndUserText.label: 'Action Start'
    i0000.begda as p0000_begda,
    @EndUserText.label: 'Action End'
    i0000.endda as p0000_endda,
    @EndUserText.label: 'Action Type'
    i0000.massn as p0000_massn,
    @EndUserText.label: 'Reason for Action'
    i0000.massg as p0000_massg,
    @EndUserText.label: 'Employment Status'
    i0000.stat2 as p0000_stat2,
    @EndUserText.label: 'Personal dtl. Start'
    i0002.begda as p0002_begda,
    @EndUserText.label: 'Personal dtl. End'
    i0002.endda as p0002_endda,
    @EndUserText.label: 'First Name'
    i0002.vorna as p0002_vorna,
    @EndUserText.label: 'Last Name'
    i0002.nachn as p0002_nachn,
    @EndUserText.label: 'Initial'
    i0002.inits as p0002_inits,
    @EndUserText.label: 'Nickname'
    i0002.rufnm as p0002_rufnm,
    @EndUserText.label: 'Date of Birth'
    i0002.gbdat as p0002_gbdat,
    @EndUserText.label: 'Language'
    i0002.sprsl as p0002_sprsl,
    @EndUserText.label: 'Nationality'
    i0002.natio as p0002_natio,
    @EndUserText.label: 'Gender'
    i0002.gesch as p0002_gesch,
    @EndUserText.label: 'Org. Assign. Start'
    i0001.begda as p0001_begda,
    @EndUserText.label: 'Org. Assign. End'
    i0001.endda as p0001_endda,
    @EndUserText.label: 'Company Code'
    i0001.bukrs as p0001_bukrs,
    @EndUserText.label: 'Cost Centre'
    i0001.kostl as p0001_kostl,
    @EndUserText.label: 'Personnel Area'
    i0001.werks as p0001_werks,
    @EndUserText.label: 'Payroll Area'
    i0001.abkrs as p0001_abrks,
    @EndUserText.label: 'Position'
    i0001.plans as p0001_plans,
    @EndUserText.label: 'Employee Group'
    i0001.persg as p0001_persg,
    @EndUserText.label: 'Employee Sub Group'
    i0001.persk as p0001_persk,
    @EndUserText.label: 'Org. Key'
    i0001.vdsk1 as p0001_vdsk1,
    @EndUserText.label: 'Org. Unit'
    i0001.orgeh as p0001_orgeh
    
}
where ( msg.procstat = '1' and
 i0000.massn <> 'XX')
group by msg.client, msg.mssgguid, msg.pernr, msg.infotyp, msg.msgtstamp, msg.procstat, i0000.begda, i0000.endda, i0000.massn, i0000.massg, i0000.stat2, i0002.begda,
i0002.endda, i0002.vorna, i0002.nachn, i0002.inits, i0002.rufnm, i0002.gbdat, i0002.sprsl, i0002.natio, i0002.gesch, i0001.begda, i0001.endda, i0001.bukrs,
i0001.kostl, i0001.werks, i0001.abkrs, i0001.plans, i0001.persg, i0001.persk, i0001.vdsk1, i0001.orgeh

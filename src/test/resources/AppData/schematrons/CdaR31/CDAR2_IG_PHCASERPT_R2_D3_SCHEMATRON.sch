<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<!--

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL LANTANA CONSULTING GROUP LLC, OR ANY OF THEIR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
Schematron generated from Trifolia on 11/16/2021
-->
<sch:schema xmlns:voc="http://www.lantanagroup.com/voc" xmlns:svs="urn:ihe:iti:svs:2008" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:sdtc="urn:hl7-org:sdtc" xmlns="urn:hl7-org:v3" xmlns:cda="urn:hl7-org:v3" xmlns:sch="http://purl.oclc.org/dsdl/schematron" queryBinding="xslt2">
  <sch:ns prefix="voc" uri="http://www.lantanagroup.com/voc" />
  <sch:ns prefix="svs" uri="urn:ihe:iti:svs:2008" />
  <sch:ns prefix="xsi" uri="http://www.w3.org/2001/XMLSchema-instance" />
  <sch:ns prefix="sdtc" uri="urn:hl7-org:sdtc" />
  <sch:ns prefix="cda" uri="urn:hl7-org:v3" />
  <sch:phase id="errors">
    <sch:active pattern="p-validate_document-level-templateId" />
    <sch:active pattern="p-validate_CD_CE" />
    <sch:active pattern="p-validate_code_codesystem_CD_CE" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-CLOSEDTEMPLATE" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.1-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.2-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.24-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.31-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.32-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.37-errors" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-errors" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.2.12-errors" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.53-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.64-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.4-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.72-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.109-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.111-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.123-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.113-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.121-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.130-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.141-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.143-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.119-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.122-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.118-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.129-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.120-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-CLOSEDTEMPLATE" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-errors" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-errors" />
  </sch:phase>
  <sch:phase id="warnings">
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.1-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.2-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.24-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.31-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.32-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.37-warnings" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-warnings" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.2.12-warnings" />
    <sch:active pattern="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.53-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.64-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.5.4-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.72-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.109-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.111-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.123-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.113-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.121-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.130-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.141-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.143-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.119-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.122-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.118-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.129-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.120-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings" />
    <sch:active pattern="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-warnings" />
    <sch:active pattern="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-warnings" />
  </sch:phase>
  <sch:pattern id="p-validate_document-level-templateId">
    <sch:rule id="r-validate_document-level-templateId-fatal-abstract" abstract="true" role="fatal">
      <sch:assert test="count(cda:ClinicalDocument/cda:templateId[@root='2.16.840.1.113883.10.20.15.2'][@extension='2021-01-01'])=1">Fatal: SHALL contain exactly one [1..1] templateId (CONF:4482-96) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2" eICR Initial Public Health Case Report Document (CONF:4482-95). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-96).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-severe-errors-validate_document-level-templateId" context="/">
      <sch:extends rule="r-validate_document-level-templateId-fatal-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-validate_CD_CE">
    <sch:rule role="error" id="r-validate_CD_CE-errors-abstract" abstract="true">
      <sch:assert id="a-validate_CD_CE-c" test="(parent::cda:regionOfInterest) or ((@code or @nullFlavor or (@codeSystem and @nullFlavor='OTH')) and not(@code and @nullFlavor) and not(@codeSystem and @nullFlavor!='OTH'))">
        Data types of CD or CE SHALL have either @code or @nullFlavor or both (@codeSystem and @nullFlavor="OTH") but SHALL NOT have both @code and @nullFlavor and SHALL NOT have @codeSystem and @nullFlavor not equal to "OTH" (Rule: validate_CD_CE)</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-errors-validate_CD_CE" context="//cda:code|cda:value[@xsi:type='CD']|cda:value[@xsi:type='CE']|cda:administrationUnitCode|cda:administrativeGenderCode|cda:awarenessCode|cda:confidentialityCode|cda:dischargeDispositionCode|cda:ethnicGroupCode|cda:functionCode|cda:interpretationCode|cda:maritalStatusCode|cda:methodCode|cda:modeCode|cda:priorityCode|cda:proficiencyLevelCode|cda:RaceCode|cda:religiousAffiliationCode|cda:routeCode|cda:standardIndustryClassCode">
      <sch:extends rule="r-validate_CD_CE-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-validate_code_codesystem_CD_CE">
    <sch:rule role="error" id="r-validate_code_codesystem_CD_CE-errors-abstract" abstract="true">
      <sch:assert id="a-validate_code_codesystem_CD_CE-c" test="(parent::cda:regionOfInterest) or ((@code and @codeSystem) or not(@code))"> Data types of CD or CE SHALL NOT have @code without @codeSystem (Rule: validate_code_codesystem_CD_CE)</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-errors-validate_code_codesystem_CD_CE" context="//cda:code|cda:value[@xsi:type='CD']|cda:value[@xsi:type='CE']|cda:administrationUnitCode|cda:administrativeGenderCode|cda:awarenessCode|cda:confidentialityCode|cda:dischargeDispositionCode|cda:ethnicGroupCode|cda:functionCode|cda:interpretationCode|cda:maritalStatusCode|cda:methodCode|cda:modeCode|cda:priorityCode|cda:proficiencyLevelCode|cda:RaceCode|cda:religiousAffiliationCode|cda:routeCode|cda:standardIndustryClassCode">
      <sch:extends rule="r-validate_code_codesystem_CD_CE-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors-abstract" abstract="true">
      <sch:assert id="a-81-444" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-444).</sch:assert>
      <sch:assert id="a-81-445" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-445).</sch:assert>
      <sch:assert id="a-81-448" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:81-448).</sch:assert>
      <sch:assert id="a-81-450" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:81-450).</sch:assert>
      <sch:assert id="a-81-19096" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:81-19096).</sch:assert>
      <sch:assert id="a-81-19139" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-19139).</sch:assert>
      <sch:assert id="a-81-19140" test="cda:code[@code='11778-8']">This code SHALL contain exactly one [1..1] @code="11778-8" Estimated date of delivery (CONF:81-19140).</sch:assert>
      <sch:assert id="a-81-26503" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:81-26503).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.3.1']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors-abstract" />
      <sch:assert id="a-81-16762" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.3.1'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-16762) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.3.1" (CONF:81-16763).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-CLOSEDTEMPLATE">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors-CL-abstract" abstract="true">
      <sch:assert id="a-81-180-CL" test="count(.//cda:templateId[@root != '2.16.840.1.113883.10.20.15.3.1'])=0">'urn:oid:2.16.840.1.113883.10.20.15.3.1' is a closed template, only defined templates are allowed.</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors-CL" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.3.1']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-errors-CL-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors-abstract" abstract="true">
      <sch:assert id="a-81-451" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-451).</sch:assert>
      <sch:assert id="a-81-452" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-452).</sch:assert>
      <sch:assert id="a-81-455" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:81-455).</sch:assert>
      <sch:assert id="a-81-457" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Extended Pregnancy Status urn:oid:2.16.840.1.113762.1.4.1099.24 DYNAMIC (CONF:81-457).</sch:assert>
      <sch:assert id="a-81-19110" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:81-19110).</sch:assert>
      <sch:assert id="a-81-19153" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-19153).</sch:assert>
      <sch:assert id="a-81-19154" test="cda:code[@code='ASSERTION']">This code SHALL contain exactly one [1..1] @code="ASSERTION" Assertion (CONF:81-19154).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.3.8']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors-abstract" />
      <sch:assert id="a-81-16768" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.3.8'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-16768) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.3.8" (CONF:81-16868).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.1-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-errors-abstract" abstract="true">
      <sch:assert id="a-81-7157-c" test="count(cda:given) &gt; 0 or @nullFlavor">SHALL contain at least one [1..*] given (CONF:81-7157).</sch:assert>
      <sch:assert id="a-81-7159-c" test="count(cda:family)=1 or @nullFlavor">SHALL contain exactly one [1..1] family (CONF:81-7159).</sch:assert>
      <sch:assert id="a-81-7278-c" test="not(text()[normalize-space()])">**SHALL NOT** have mixed content except for white space (CONF:81-7278).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14' and @extension='2015-08-01']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.2' and @extension='2017-08-01']]/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.5.1.1.3' and @extension='2019-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.5.1.1.3' and @extension='2021-10-14']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.2-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-errors-abstract" abstract="true">
      <sch:assert id="a-81-7291-c" test="@nullFlavor or (count(cda:streetAddressLine) &gt;= 1 and count(cda:streetAddressLine) &lt;= 4)">SHALL contain at least one and not more than 4 streetAddressLine (CONF:81-7291).</sch:assert>
      <sch:assert id="a-81-7292-c" test="count(cda:city)=1 or @nullFlavor">SHALL contain exactly one [1..1] city (CONF:81-7292).</sch:assert>
      <sch:assert id="a-81-7296-c" test="not(text()[normalize-space()])">**SHALL NOT** have mixed content except for white space (CONF:81-7296).</sch:assert>
      <sch:assert id="a-81-10024-c" test="@nullFlavor or (cda:country='US' and cda:state) or (cda:country!='US') or (not(cda:country) and cda:state) ">If the country is US, the state element is required but SHOULD have @nullFlavor if the state is unknown. If country is not specified, it's assumed to be US. If country is something other than US, the state MAY be present but MAY be bound to different vocabularies (CONF:81-10024).</sch:assert>
      <sch:assert id="a-81-10025-c" test="@nullFlavor or (cda:country='US' and cda:postalCode) or (cda:country!='US') or (not(cda:country) and cda:postalCode)">If the country is US, the postalCode element is required but SHOULD have @nullFlavor if the postalCode is unknown. If country is not specified, it's assumed to be US. If country is something other than US, the postalCode MAY be present but MAY be bound to different vocabularies (CONF:81-10025).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2014-06-09']]/cda:participant/cda:participantRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.18' and @extension='2014-06-09']]/cda:performer/cda:assignedEntity/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2014-06-09']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.2.3.4']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.15' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:addr | cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.13.20' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.3.5416.1.8981.1.1']]/cda:recordTarget/cda:patientRole/cda:addr | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2015-08-01']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:performer[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.1' and @extension='2017-08-01']]/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.2' and @extension='2017-08-01']]/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2017-05-01']]/cda:participant/cda:participantRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='https://mytest.Header123']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:receivedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='https://icHeader.abc-orig']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.3.5416.1.8981.1.1']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.7' and @extension='2020-05-19']]/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:participant/cda:associatedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:participant/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:participant/cda:associatedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:participant/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:addr | cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.7' and @extension='2021-11-09']]/cda:associatedEntity/cda:scopingOrganization/cda:addr">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.24-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-errors-abstract" abstract="true">
      <sch:assert id="a-81-7490" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:81-7490).</sch:assert>
      <sch:assert id="a-81-7492" test="count(cda:playingEntity)=1">SHALL contain exactly one [1..1] playingEntity (CONF:81-7492).</sch:assert>
      <sch:assert id="a-81-7493" test="cda:playingEntity[count(cda:code)=1]">This playingEntity SHALL contain exactly one [1..1] code (CONF:81-7493).</sch:assert>
      <sch:assert id="a-81-19137" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-19137).</sch:assert>
      <sch:assert id="a-81-19138" test="cda:code[@code='412307009']">This code SHALL contain exactly one [1..1] @code="412307009" Drug Vehicle (CONF:81-19138).</sch:assert>
      <sch:assert id="a-81-26502" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:81-26502).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-errors" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.24']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-errors-abstract" />
      <sch:assert id="a-81-7495" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.24'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7495) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.24" (CONF:81-10493).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.31-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-errors-abstract" abstract="true">
      <sch:assert id="a-81-7613" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-7613).</sch:assert>
      <sch:assert id="a-81-7614" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-7614).</sch:assert>
      <sch:assert id="a-81-7615" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-7615).</sch:assert>
      <sch:assert id="a-81-7617" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:81-7617).</sch:assert>
      <sch:assert id="a-81-7618" test="cda:value[@xsi:type='PQ'][@unit]">This value SHALL contain exactly one [1..1] @unit, which SHALL be selected from ValueSet AgePQ_UCUM urn:oid:2.16.840.1.113883.11.20.9.21 DYNAMIC (CONF:81-7618).</sch:assert>
      <sch:assert id="a-81-15965" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:81-15965).</sch:assert>
      <sch:assert id="a-81-15966" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:81-15966).</sch:assert>
      <sch:assert id="a-81-16776" test="cda:code[@code='445518008']">This code SHALL contain exactly one [1..1] @code="445518008" Age At Onset (CONF:81-16776).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.31']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-errors-abstract" />
      <sch:assert id="a-81-7899" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.31'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7899) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.31" (CONF:81-10487).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.32-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-errors-abstract" abstract="true">
      <sch:assert id="a-81-7758" test="@classCode='SDLOC'">SHALL contain exactly one [1..1] @classCode="SDLOC" (CodeSystem: HL7RoleCode urn:oid:2.16.840.1.113883.5.111 STATIC) (CONF:81-7758).</sch:assert>
      <sch:assert id="a-81-7763" test="not(cda:playingEntity) or cda:playingEntity[@classCode='PLC']">The playingEntity, if present, SHALL contain exactly one [1..1] @classCode="PLC" (CodeSystem: HL7EntityClass urn:oid:2.16.840.1.113883.5.41 STATIC) (CONF:81-7763).</sch:assert>
      <sch:assert id="a-81-16850" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet HealthcareServiceLocation urn:oid:2.16.840.1.113883.1.11.20275 DYNAMIC (CONF:81-16850).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-errors" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.32']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-errors-abstract" />
      <sch:assert id="a-81-7635" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.32'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7635) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.32" (CONF:81-10524).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.37-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-errors-abstract" abstract="true">
      <sch:assert id="a-81-7900" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" Manufactured Product (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:81-7900).</sch:assert>
      <sch:assert id="a-81-7902" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:81-7902).</sch:assert>
      <sch:assert id="a-81-7903" test="count(cda:playingDevice)=1">SHALL contain exactly one [1..1] playingDevice (CONF:81-7903).</sch:assert>
      <sch:assert id="a-81-7905" test="count(cda:scopingEntity)=1">SHALL contain exactly one [1..1] scopingEntity (CONF:81-7905).</sch:assert>
      <sch:assert id="a-81-7908" test="cda:scopingEntity[count(cda:id) &gt; 0]">This scopingEntity SHALL contain at least one [1..*] id (CONF:81-7908).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-errors" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.37']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-errors-abstract" />
      <sch:assert id="a-81-7901" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.37'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7901) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.37" (CONF:81-10522).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-errors">
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-errors-abstract" abstract="true">
      <sch:assert id="a-81-7814" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:81-7814).</sch:assert>
      <sch:assert id="a-81-7815" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-7815).</sch:assert>
      <sch:assert id="a-81-15435" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-15435).</sch:assert>
      <sch:assert id="a-81-15436" test="cda:code[@code='10187-3']">This code SHALL contain exactly one [1..1] @code="10187-3" Review of Systems (CONF:81-15436).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-errors" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.18']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-errors-abstract" />
      <sch:assert id="a-81-7812" test="count(cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.18'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7812) such that it SHALL contain exactly one [1..1] @root="1.3.6.1.4.1.19376.1.5.3.1.3.18" (CONF:81-10469).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-errors">
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-errors-abstract" abstract="true">
      <sch:assert id="a-81-7834" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:81-7834).</sch:assert>
      <sch:assert id="a-81-7835" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-7835).</sch:assert>
      <sch:assert id="a-81-15451" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-15451).</sch:assert>
      <sch:assert id="a-81-15452" test="cda:code[@code='10154-3']">This code SHALL contain exactly one [1..1] @code="10154-3" Chief Complaint (CONF:81-15452).</sch:assert>
      <sch:assert id="a-81-26474" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:81-26474).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-errors" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-errors-abstract" />
      <sch:assert id="a-81-7832" test="count(cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7832) such that it SHALL contain exactly one [1..1] @root="1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1" (CONF:81-10453).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.2.12-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-errors-abstract" abstract="true">
      <sch:assert id="a-81-7838" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:81-7838).</sch:assert>
      <sch:assert id="a-81-7839" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-7839).</sch:assert>
      <sch:assert id="a-81-15429" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-15429).</sch:assert>
      <sch:assert id="a-81-15430" test="cda:code[@code='29299-5']">This code SHALL contain exactly one [1..1] @code="29299-5" Reason for Visit (CONF:81-15430).</sch:assert>
      <sch:assert id="a-81-26494" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:81-26494).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.12']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-errors-abstract" />
      <sch:assert id="a-81-7836" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.12'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7836) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.12" (CONF:81-10448).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-errors">
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-errors-abstract" abstract="true">
      <sch:assert id="a-81-7850" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:81-7850).</sch:assert>
      <sch:assert id="a-81-7851" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-7851).</sch:assert>
      <sch:assert id="a-81-15477" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-15477).</sch:assert>
      <sch:assert id="a-81-15478" test="cda:code[@code='10164-2']">This code SHALL contain exactly one [1..1] @code="10164-2" (CONF:81-15478).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-errors" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.4']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-errors-abstract" />
      <sch:assert id="a-81-7848" test="count(cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.4'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-7848) such that it SHALL contain exactly one [1..1] @root="1.3.6.1.4.1.19376.1.5.3.1.3.4" (CONF:81-10458).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.53-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-errors-abstract" abstract="true">
      <sch:assert id="a-81-8991" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-8991).</sch:assert>
      <sch:assert id="a-81-8992" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-8992).</sch:assert>
      <sch:assert id="a-81-8994" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:81-8994).</sch:assert>
      <sch:assert id="a-81-8995" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet No Immunization Reason urn:oid:2.16.840.1.113883.1.11.19717 DYNAMIC (CONF:81-8995).</sch:assert>
      <sch:assert id="a-81-8996" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:81-8996).</sch:assert>
      <sch:assert id="a-81-19104" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:81-19104).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.53']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-errors-abstract" />
      <sch:assert id="a-81-8993" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.53'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-8993) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.53" (CONF:81-10500).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-errors-abstract" abstract="true">
      <sch:assert id="a-81-9368-c" test="not(tested)">SHALL contain exactly one [1..1] name (CONF:81-9368).</sch:assert>
      <sch:assert id="a-81-9371-c" test="@nullFlavor or (cda:given and cda:family and not(text()[normalize-space()])) or (count(*)=0 and string-length(normalize-space(string(text())))!=0)">The content of name **SHALL** be either a conformant Patient Name (PTN.US.FIELDED), or a string (CONF:81-9371).</sch:assert>
      <sch:assert id="a-81-9372-c" test="@nullFlavor or (cda:given and cda:family and not(text()[normalize-space()])) or (count(*)=0 and string-length(normalize-space(string(text())))!=0)">The string **SHALL NOT** contain name parts (CONF:81-9372).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2014-06-09']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:encounterParticipant[cda:templateId[@root='2.16.840.1.113883.10.20.6.2.2' and @extension='2014-06-09']]/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.5' and @extension='2014-06-09']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.26' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.25' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.15' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.13.20' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-11-19']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-11-19']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-12-01']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-12-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2015-08-01']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.5' and @extension='2015-08-01']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-10-05']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-10-05']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.202' and @extension='2016-11-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-11-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-11-29']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2017-05-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.9.275.1' and @extension='2021-12-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.500.1' and @extension='2022-06-01']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.64-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-errors-abstract" abstract="true">
      <sch:assert id="a-81-9425" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-9425).</sch:assert>
      <sch:assert id="a-81-9426" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-9426).</sch:assert>
      <sch:assert id="a-81-9428" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-9428).</sch:assert>
      <sch:assert id="a-81-9430" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-9430).</sch:assert>
      <sch:assert id="a-81-9431-c" test="not(tested-here)">This text SHALL contain exactly one [1..1] reference/@value (CONF:81-9431).</sch:assert>
      <sch:assert id="a-81-15967" test="cda:text[count(cda:reference)=1]">This text SHALL contain exactly one [1..1] reference (CONF:81-15967).</sch:assert>
      <sch:assert id="a-81-15968" test="cda:text/cda:reference[@value]">This reference SHALL contain exactly one [1..1] @value (CONF:81-15968).</sch:assert>
      <sch:assert id="a-81-15969-c" test="count(cda:text/cda:reference[@value])=0 or starts-with(cda:text/cda:reference/@value, '#')">This reference/@value SHALL begin with a '#' and SHALL point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:81-15969).</sch:assert>
      <sch:assert id="a-81-19159" test="cda:code[@code='48767-8']">This code SHALL contain exactly one [1..1] @code="48767-8" Annotation Comment (CONF:81-19159).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.64']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-errors-abstract" />
      <sch:assert id="a-81-9427" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.64'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-9427) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.64" (CONF:81-10491).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.4-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-errors-abstract" abstract="true">
      <sch:assert id="a-81-10127-c" test="string-length(@value)&gt;=8">**SHALL** be precise to the day (CONF:81-10127).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:author/cda:time">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.72-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-errors-abstract" abstract="true">
      <sch:assert id="a-81-14219" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-14219).</sch:assert>
      <sch:assert id="a-81-14220" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:81-14220).</sch:assert>
      <sch:assert id="a-81-14223" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:81-14223).</sch:assert>
      <sch:assert id="a-81-14227" test="count(cda:participant) &gt; 0">SHALL contain at least one [1..*] participant (CONF:81-14227).</sch:assert>
      <sch:assert id="a-81-14228" test="cda:participant[count(cda:participantRole)=1]">Such participants SHALL contain exactly one [1..1] participantRole (CONF:81-14228).</sch:assert>
      <sch:assert id="a-81-14229" test="cda:participant/cda:participantRole[@classCode='CAREGIVER']">This participantRole SHALL contain exactly one [1..1] @classCode="CAREGIVER" (CONF:81-14229).</sch:assert>
      <sch:assert id="a-81-14230" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:81-14230).</sch:assert>
      <sch:assert id="a-81-14233" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:81-14233).</sch:assert>
      <sch:assert id="a-81-14599" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD" (CONF:81-14599).</sch:assert>
      <sch:assert id="a-81-14831" test="not(cda:participant/cda:time) or cda:participant/cda:time[count(cda:low)=1]">The time, if present, SHALL contain exactly one [1..1] low (CONF:81-14831).</sch:assert>
      <sch:assert id="a-81-19090" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:81-19090).</sch:assert>
      <sch:assert id="a-81-26451" test="cda:participant[@typeCode='IND']">Such participants SHALL contain exactly one [1..1] @typeCode="IND" (CONF:81-26451).</sch:assert>
      <sch:assert id="a-81-14600-c" test="cda:value/@codeSystem='2.16.840.1.113883.6.1' or cda:value/@codeSystem='2.16.840.1.113883.6.96' or cda:value[@nullFlavor and not(@codeSystem)]">The code **SHALL** be selected from LOINC (codeSystem: 2.16.840.1.113883.6.1) or SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96) (CONF:81-14600).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.72']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-errors-abstract" />
      <sch:assert id="a-81-14221" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.72'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-14221) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.72" (CONF:81-14222).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.109-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-errors-abstract" abstract="true">
      <sch:assert id="a-1098-27890" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-27890).</sch:assert>
      <sch:assert id="a-1098-27891" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-27891).</sch:assert>
      <sch:assert id="a-1098-27892" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.109'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-27892) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.109" (CONF:1098-27893).</sch:assert>
      <sch:assert id="a-1098-27894" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-27894).</sch:assert>
      <sch:assert id="a-1098-27901" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-27901).</sch:assert>
      <sch:assert id="a-1098-27902" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-27902).</sch:assert>
      <sch:assert id="a-1098-28823" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Residence and Accommodation Type urn:oid:2.16.840.1.113883.11.20.9.49 DYNAMIC (CONF:1098-28823).</sch:assert>
      <sch:assert id="a-1098-31352" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31352).</sch:assert>
      <sch:assert id="a-1098-31353" test="cda:code[@code='75274-1']">This code SHALL contain exactly one [1..1] @code="75274-1" Characteristics of residence (CONF:1098-31353).</sch:assert>
      <sch:assert id="a-1098-31354" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-31354).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.109']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.111-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-errors-abstract" abstract="true">
      <sch:assert id="a-1098-27924" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-27924).</sch:assert>
      <sch:assert id="a-1098-27925" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-27925).</sch:assert>
      <sch:assert id="a-1098-27926" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.111'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-27926) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.111" (CONF:1098-27927).</sch:assert>
      <sch:assert id="a-1098-27928" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-27928).</sch:assert>
      <sch:assert id="a-1098-27929" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-27929).</sch:assert>
      <sch:assert id="a-1098-27930" test="cda:code[@code='75281-6']">This code SHALL contain exactly one [1..1] @code="75281-6" Personal belief (CONF:1098-27930).</sch:assert>
      <sch:assert id="a-1098-27931" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-27931).</sch:assert>
      <sch:assert id="a-1098-27936" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-27936).</sch:assert>
      <sch:assert id="a-1098-27937" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-27937).</sch:assert>
      <sch:assert id="a-1098-28442" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:1098-28442).</sch:assert>
      <sch:assert id="a-1098-32487-c" test="(cda:value[@xsi:type='CD'][@codeSystem='2.16.840.1.113883.6.96']) or (count(cda:value[@xsi:type='CD'])=0)">If xsi:type is CD, **SHALL** contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED-CT  urn:oid:2.16.840.1.113883.6.96 STATIC) (CONF:1098-32487).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.111']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7496" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7496).</sch:assert>
      <sch:assert id="a-1098-7497" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.18']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet MoodCodeEvnInt urn:oid:2.16.840.1.113883.11.20.9.18 STATIC 2011-04-03 (CONF:1098-7497).</sch:assert>
      <sch:assert id="a-1098-7499" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7499) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.16" (CONF:1098-10504). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32498).</sch:assert>
      <sch:assert id="a-1098-7500" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7500).</sch:assert>
      <sch:assert id="a-1098-7507" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7507).</sch:assert>
      <sch:assert id="a-1098-7508-c" test="count(cda:effectiveTime[(cda:low or @value) and not(cda:low and @value)])=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-7508) such that it</sch:assert>
      <sch:assert id="a-1098-28499-c" test="count(cda:effectiveTime[@operator='A'])=0 or count(cda:effectiveTime[@operator='A' and (@xsi:type='PIVL_TS' or @xsi:type='EIVL_TS')])=1">**SHALL** contain exactly one [1..1] @xsi:type="PIVL_TS" or "EIVL_TS" (CONF:1098-28499).</sch:assert>
      <sch:assert id="a-1098-7516" test="count(cda:doseQuantity)=1">SHALL contain exactly one [1..1] doseQuantity (CONF:1098-7516).</sch:assert>
      <sch:assert id="a-1098-7525" test="not(cda:rateQuantity) or cda:rateQuantity[@unit]">The rateQuantity, if present, SHALL contain exactly one [1..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-7525).</sch:assert>
      <sch:assert id="a-1098-7520" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:1098-7520).</sch:assert>
      <sch:assert id="a-1098-16085" test="cda:consumable[count(cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23' and @extension='2014-06-09']])=1]">This consumable SHALL contain exactly one [1..1] Medication Information (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.23:2014-06-09) (CONF:1098-16085).</sch:assert>
      <sch:assert id="a-1098-31882" test="not(cda:precondition) or cda:precondition[@typeCode='PRCN']">The precondition, if present, SHALL contain exactly one [1..1] @typeCode="PRCN" (CONF:1098-31882).</sch:assert>
      <sch:assert id="a-1098-31883" test="not(cda:precondition) or cda:precondition[count(cda:criterion[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.25' and @extension='2014-06-09']])=1]">The precondition, if present, SHALL contain exactly one [1..1] Precondition for Substance Administration (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.25:2014-06-09) (CONF:1098-31883).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-errors-abstract" abstract="true">
      <sch:assert id="a-1098-32890-branch-7508-c" test="not(tested_here)">This effectiveTime **SHALL** contain either a low or a @value but not both (CONF:1098-32890).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']]/cda:effectiveTime">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors">
    <!--Pattern is used in an implied relationship.-->
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-15385" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15385).</sch:assert>
      <sch:assert id="a-1098-15386" test="cda:code[@code='10160-0']">This code SHALL contain exactly one [1..1] @code="10160-0" History of medication use (CONF:1098-15386).</sch:assert>
      <sch:assert id="a-1098-7793-c" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-7793).</sch:assert>
      <sch:assert id="a-1098-7794" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-7794).</sch:assert>
      <sch:assert id="a-1098-30824" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30824).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7791" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7791) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.1" (CONF:1098-10432). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32500).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-15387" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15387).</sch:assert>
      <sch:assert id="a-1098-15388" test="cda:code[@code='10160-0']">This code SHALL contain exactly one [1..1] @code="10160-0" History of medication use (CONF:1098-15388).</sch:assert>
      <sch:assert id="a-1098-7570" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-7570).</sch:assert>
      <sch:assert id="a-1098-7571" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-7571).</sch:assert>
      <sch:assert id="a-1098-7572-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16'][@extension='2014-06-09']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1098-7572) such that it SHALL contain exactly one [1..1] Medication Activity (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.16:2014-06-09) (CONF:1098-10077).</sch:assert>
      <sch:assert id="a-1098-30825" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30825).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7568" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1.1'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7568) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.1.1" (CONF:1098-10433). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32499).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-14749" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-14749).</sch:assert>
      <sch:assert id="a-1098-14750" test="cda:code[@code='18776-5']">This code SHALL contain exactly one [1..1] @code="18776-5" Plan of Treatment (CONF:1098-14750).</sch:assert>
      <sch:assert id="a-1098-16986" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-16986).</sch:assert>
      <sch:assert id="a-1098-7725" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-7725).</sch:assert>
      <sch:assert id="a-1098-30813" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30813).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.10' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7723" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.10'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7723) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.10" (CONF:1098-10435). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32501).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.123-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-errors-abstract" abstract="true">
      <sch:assert id="a-1098-28656" test="@moodCode='INT'">SHALL contain exactly one [1..1] @moodCode="INT" (CONF:1098-28656).</sch:assert>
      <sch:assert id="a-1098-28660" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-28660).</sch:assert>
      <sch:assert id="a-1098-28661" test="count(cda:participant) &gt; 0">SHALL contain at least one [1..*] participant (CONF:1098-28661) such that it</sch:assert>
      <sch:assert id="a-1098-30818" test="cda:code[@code='395170001']">This code SHALL contain exactly one [1..1] @code="395170001" medication monitoring (regime/therapy) (CONF:1098-30818).</sch:assert>
      <sch:assert id="a-1098-30819" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:1098-30819).</sch:assert>
      <sch:assert id="a-1098-30823" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-30823).</sch:assert>
      <sch:assert id="a-1098-31920" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-31920).</sch:assert>
      <sch:assert id="a-1098-31921" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31921).</sch:assert>
      <sch:assert id="a-1098-31922" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-31922).</sch:assert>
      <sch:assert id="a-1098-32358" test="cda:statusCode[@code]">This statusCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet ActStatus urn:oid:2.16.840.1.113883.1.11.15933 DYNAMIC (CONF:1098-32358).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-errors-abstract" />
      <sch:assert id="a-1098-28657" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-28657) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.123" (CONF:1098-28658).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-28661-branch-28661-errors-abstract" abstract="true">
      <sch:assert id="a-1098-28662-branch-28661" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:1098-28662). This participantRole SHALL contain exactly one [1..1] @classCode="ASSIGNED" (CONF:1098-28664).</sch:assert>
      <sch:assert id="a-1098-28665-branch-28661" test="cda:participantRole[count(cda:id) &gt; 0]">This participantRole SHALL contain at least one [1..*] id (CONF:1098-28665).</sch:assert>
      <sch:assert id="a-1098-28667-branch-28661" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:1098-28667).</sch:assert>
      <sch:assert id="a-1098-28668-branch-28661" test="cda:participantRole/cda:playingEntity[@classCode='PSN']">This playingEntity SHALL contain exactly one [1..1] @classCode="PSN" (CONF:1098-28668).</sch:assert>
      <sch:assert id="a-1098-28669-branch-28661-c" test="cda:participantRole/cda:playingEntity[count(cda:name)=1]">This playingEntity SHALL contain exactly one [1..1] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1098-28669).</sch:assert>
      <sch:assert id="a-1098-28663-branch-28661" test="@typeCode='RESP'">SHALL contain exactly one [1..1] @typeCode="RESP" (CONF:1098-28663).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-28661-branch-28661-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123']]/cda:participant">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-28661-branch-28661-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.113-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-errors-abstract" abstract="true">
      <sch:assert id="a-1098-29035" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-29035).</sch:assert>
      <sch:assert id="a-1098-29036" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-29036).</sch:assert>
      <sch:assert id="a-1098-29039" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-29039).</sch:assert>
      <sch:assert id="a-1098-29469" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:1098-29469).</sch:assert>
      <sch:assert id="a-1098-31123" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-31123).</sch:assert>
      <sch:assert id="a-1098-31350" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31350).</sch:assert>
      <sch:assert id="a-1098-31351" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-31351).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.113']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-errors-abstract" />
      <sch:assert id="a-1098-29037" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.113'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-29037) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.113" (CONF:1098-29038).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7325" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7325).</sch:assert>
      <sch:assert id="a-1098-7326" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7326).</sch:assert>
      <sch:assert id="a-1098-7323" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.9'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7323) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.9" (CONF:1098-10523). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32504).</sch:assert>
      <sch:assert id="a-1098-7329" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7329).</sch:assert>
      <sch:assert id="a-1098-16851" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-16851).</sch:assert>
      <sch:assert id="a-1098-7328" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7328).</sch:assert>
      <sch:assert id="a-1098-19114" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19114).</sch:assert>
      <sch:assert id="a-1098-7335" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Problem urn:oid:2.16.840.1.113883.3.88.12.3221.7.4 DYNAMIC (CONF:1098-7335).</sch:assert>
      <sch:assert id="a-1098-31124" test="cda:code[@code='ASSERTION']">This code SHALL contain exactly one [1..1] @code="ASSERTION" (CONF:1098-31124).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.9' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8289" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8289).</sch:assert>
      <sch:assert id="a-1098-8290" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-8290).</sch:assert>
      <sch:assert id="a-1098-8291" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.12'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-8291) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.12" (CONF:1098-10519). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32505).</sch:assert>
      <sch:assert id="a-1098-8292" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8292).</sch:assert>
      <sch:assert id="a-1098-8293" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-8293).</sch:assert>
      <sch:assert id="a-1098-19189-c" test="count(cda:code/cda:originalText/cda:reference[@value])=0 or starts-with(cda:code/cda:originalText/cda:reference/@value, '#')">This reference/@value **SHALL** begin with a '#' and **SHALL** point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:1098-19189).</sch:assert>
      <sch:assert id="a-1098-8298" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-8298).</sch:assert>
      <sch:assert id="a-1098-8299" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-8299).</sch:assert>
      <sch:assert id="a-1098-8302" test="not(cda:performer) or cda:performer[count(cda:assignedEntity)=1]">The performer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1098-8302).</sch:assert>
      <sch:assert id="a-1098-8303" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:1098-8303).</sch:assert>
      <sch:assert id="a-1098-8304" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:addr) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] addr (CONF:1098-8304).</sch:assert>
      <sch:assert id="a-1098-8305" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:telecom) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] telecom (CONF:1098-8305).</sch:assert>
      <sch:assert id="a-1098-8310" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:telecom) &gt; 0]">The representedOrganization, if present, SHALL contain at least one [1..*] telecom (CONF:1098-8310).</sch:assert>
      <sch:assert id="a-1098-8309" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:addr) &gt; 0]">The representedOrganization, if present, SHALL contain at least one [1..*] addr (CONF:1098-8309).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.12' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7652" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" Procedure (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7652).</sch:assert>
      <sch:assert id="a-1098-7653" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7653).</sch:assert>
      <sch:assert id="a-1098-7654" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7654) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.14" (CONF:1098-10521). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32506).</sch:assert>
      <sch:assert id="a-1098-7655" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7655).</sch:assert>
      <sch:assert id="a-1098-7656" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-7656).</sch:assert>
      <sch:assert id="a-1098-19206-c" test="count(cda:code/cda:originalText/cda:reference[@value])=0 or starts-with(cda:code/cda:originalText/cda:reference/@value, '#')">This reference/@value **SHALL** begin with a '#' and **SHALL** point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:1098-19206).</sch:assert>
      <sch:assert id="a-1098-7661" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7661).</sch:assert>
      <sch:assert id="a-1098-7890-c" test="not(testable)">MethodCode **SHALL NOT** conflict with the method inherent in Procedure / code (CONF:1098-7890).</sch:assert>
      <sch:assert id="a-1098-7704" test="not(cda:specimen) or cda:specimen[count(cda:specimenRole)=1]">The specimen, if present, SHALL contain exactly one [1..1] specimenRole (CONF:1098-7704).</sch:assert>
      <sch:assert id="a-1098-16842-c" test="not(tested)">This specimen is for representing specimens obtained from a procedure (CONF:1098-16842).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7737-branch-7718" test="not(cda:assignedEntity/cda:representedOrganization) or cda:assignedEntity/cda:representedOrganization[count(cda:telecom)=1]">The representedOrganization, if present, SHALL contain exactly one [1..1] telecom (CONF:1098-7737).</sch:assert>
      <sch:assert id="a-1098-7736-branch-7718" test="not(cda:assignedEntity/cda:representedOrganization) or cda:assignedEntity/cda:representedOrganization[count(cda:addr)=1]">The representedOrganization, if present, SHALL contain exactly one [1..1] addr (CONF:1098-7736).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14' and @extension='2014-06-09']]/cda:performer[cda:assignedEntity[cda:id][cda:addr][cda:telecom]]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8282" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8282).</sch:assert>
      <sch:assert id="a-1098-8237" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-8237).</sch:assert>
      <sch:assert id="a-1098-8238" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.13'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-8238) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.13" (CONF:1098-10520). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32507).</sch:assert>
      <sch:assert id="a-1098-8239" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8239).</sch:assert>
      <sch:assert id="a-1098-19197" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-19197).</sch:assert>
      <sch:assert id="a-1098-19201-c" test="count(cda:code/cda:originalText/cda:reference[@value])=0 or starts-with(cda:code/cda:originalText/cda:reference/@value, '#')">This reference/@value **SHALL** begin with a '#' and **SHALL** point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:1098-19201).</sch:assert>
      <sch:assert id="a-1098-8245" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-8245).</sch:assert>
      <sch:assert id="a-1098-16846" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:1098-16846).</sch:assert>
      <sch:assert id="a-1098-8249-c" test="not(testable)">MethodCode **SHALL NOT** conflict with the method inherent in Observation / code (CONF:1098-8249).</sch:assert>
      <sch:assert id="a-1098-8252" test="not(cda:performer) or cda:performer[count(cda:assignedEntity)=1]">The performer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1098-8252).</sch:assert>
      <sch:assert id="a-1098-8253" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:1098-8253).</sch:assert>
      <sch:assert id="a-1098-8254" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:addr) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] addr (CONF:1098-8254).</sch:assert>
      <sch:assert id="a-1098-8255" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:telecom) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] telecom (CONF:1098-8255).</sch:assert>
      <sch:assert id="a-1098-8260" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:telecom)=1]">The representedOrganization, if present, SHALL contain exactly one [1..1] telecom (CONF:1098-8260).</sch:assert>
      <sch:assert id="a-1098-8259" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:addr)=1]">The representedOrganization, if present, SHALL contain exactly one [1..1] addr (CONF:1098-8259).</sch:assert>
      <sch:assert id="a-1098-32365" test="cda:statusCode[@code and @code=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.22']/voc:code/@value]">This statusCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet ProcedureAct statusCode urn:oid:2.16.840.1.113883.11.20.9.22 STATIC 2014-04-23 (CONF:1098-32365).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.13' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.121-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-errors-abstract" abstract="true">
      <sch:assert id="a-1098-30418" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-30418).</sch:assert>
      <sch:assert id="a-1098-30419" test="@moodCode='GOL'">SHALL contain exactly one [1..1] @moodCode="GOL" Goal (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-30419).</sch:assert>
      <sch:assert id="a-1098-30784" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30784).</sch:assert>
      <sch:assert id="a-1098-32332" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-32332).</sch:assert>
      <sch:assert id="a-1098-32333" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-32333).</sch:assert>
      <sch:assert id="a-1098-32755" test="not(cda:reference) or cda:reference[@typeCode='REFR']">The reference, if present, SHALL contain exactly one [1..1] @typeCode="REFR" Refers to (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:1098-32755).</sch:assert>
      <sch:assert id="a-1098-32756" test="not(cda:reference) or cda:reference[count(cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.115' and @extension='2014-06-09']])=1]">The reference, if present, SHALL contain exactly one [1..1] External Document Reference (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.115:2014-06-09) (CONF:1098-32756).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.121']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-errors-abstract" />
      <sch:assert id="a-1098-8583" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.121'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-8583) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.121" (CONF:1098-10512).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7698" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7698).</sch:assert>
      <sch:assert id="a-1098-7699" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7699).</sch:assert>
      <sch:assert id="a-1098-15518" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15518).</sch:assert>
      <sch:assert id="a-1098-15519" test="cda:code[@code='42346-7']">This code SHALL contain exactly one [1..1] @code="42346-7" Medications on Admission (CONF:1098-15519).</sch:assert>
      <sch:assert id="a-1098-7701" test="count(cda:entryRelationship[@typeCode='SUBJ'][count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:1098-7701) such that it SHALL contain exactly one [1..1] @typeCode="SUBJ" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1098-7702). SHALL contain exactly one [1..1] Medication Activity (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.16:2014-06-09) (CONF:1098-15520).</sch:assert>
      <sch:assert id="a-1098-32152" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-32152).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.36' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-16758" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.36'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-16758) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.36" (CONF:1098-16759). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32524).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-15383" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15383).</sch:assert>
      <sch:assert id="a-1098-15384" test="cda:code[@code='29549-3']">This code SHALL contain exactly one [1..1] @code="29549-3" Medications Administered (CONF:1098-15384).</sch:assert>
      <sch:assert id="a-1098-8154" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-8154).</sch:assert>
      <sch:assert id="a-1098-8155" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-8155).</sch:assert>
      <sch:assert id="a-1098-15499" test="not(cda:entry) or cda:entry[count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']])=1]">The entry, if present, SHALL contain exactly one [1..1] Medication Activity (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.16:2014-06-09) (CONF:1098-15499).</sch:assert>
      <sch:assert id="a-1098-30829" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30829).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.38' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-8152" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.38'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-8152) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.38" (CONF:1098-10405). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32525).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-15423" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15423).</sch:assert>
      <sch:assert id="a-1098-15424" test="cda:code[@code='47519-4']">This code SHALL contain exactly one [1..1] @code="47519-4" History of Procedures (CONF:1098-15424).</sch:assert>
      <sch:assert id="a-1098-17184" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-17184).</sch:assert>
      <sch:assert id="a-1098-6273" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-6273).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-6270" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-6270) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.7" (CONF:1098-6271). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32532).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-15425" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-15425).</sch:assert>
      <sch:assert id="a-1098-15426" test="cda:code[@code='47519-4']">This code SHALL contain exactly one [1..1] @code="47519-4" History of Procedures (CONF:1098-15426).</sch:assert>
      <sch:assert id="a-1098-7893" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1098-7893).</sch:assert>
      <sch:assert id="a-1098-7894" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1098-7894).</sch:assert>
      <sch:assert id="a-1098-7895-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14'][@extension='2014-06-09']])=1]) &gt; 0) or (count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.13'][@extension='2014-06-09']])=1]) &gt; 0) or (count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.12'][@extension='2014-06-09']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1098-7895) such that it MAY contain zero or one [0..1] Procedure Activity Procedure (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.14:2014-06-09) (CONF:1098-15512). MAY contain zero or one [0..1] Procedure Activity Act (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.12:2014-06-09) (CONF:1098-32877). MAY contain zero or one [0..1] Procedure Activity Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.13:2014-06-09) (CONF:1098-32878).</sch:assert>
      <sch:assert id="a-1098-31138" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-31138).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7891" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7.1'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7891) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.7.1" (CONF:1098-10447). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32533).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.130-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-errors-abstract" abstract="true">
      <sch:assert id="a-1098-30342" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Nutrition Recommendations urn:oid:2.16.840.1.113883.1.11.20.2.9 DYNAMIC (CONF:1098-30342).</sch:assert>
      <sch:assert id="a-1098-30385" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-30385).</sch:assert>
      <sch:assert id="a-1098-30386" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2014-09-01 (CONF:1098-30386).</sch:assert>
      <sch:assert id="a-1098-31697" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31697).</sch:assert>
      <sch:assert id="a-1098-31698" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-31698).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.130']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-errors-abstract" />
      <sch:assert id="a-1098-30340" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.130'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30340) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.130" (CONF:1098-30341).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8538" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8538).</sch:assert>
      <sch:assert id="a-1098-8539" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2014-09-01 (CONF:1098-8539).</sch:assert>
      <sch:assert id="a-1098-30430" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.39'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30430) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.39" (CONF:1098-30431). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32552).</sch:assert>
      <sch:assert id="a-1098-8546" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8546).</sch:assert>
      <sch:assert id="a-1098-30432" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-30432).</sch:assert>
      <sch:assert id="a-1098-31687" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31687).</sch:assert>
      <sch:assert id="a-1098-32019" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-32019).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.39' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8564" test="@classCode='ENC'">SHALL contain exactly one [1..1] @classCode="ENC" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8564).</sch:assert>
      <sch:assert id="a-1098-8565" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2014-09-01 (CONF:1098-8565).</sch:assert>
      <sch:assert id="a-1098-30437" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.40'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30437) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.40" (CONF:1098-30438). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32553).</sch:assert>
      <sch:assert id="a-1098-8567" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8567).</sch:assert>
      <sch:assert id="a-1098-30439" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-30439).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-errors" context="cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.40' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8568" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8568).</sch:assert>
      <sch:assert id="a-1098-8569" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2011-09-30 (CONF:1098-8569).</sch:assert>
      <sch:assert id="a-1098-30444" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.41'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30444) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.41" (CONF:1098-30445). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32554).</sch:assert>
      <sch:assert id="a-1098-8571" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8571).</sch:assert>
      <sch:assert id="a-1098-30446" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-30446).</sch:assert>
      <sch:assert id="a-1098-31976" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31976).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.41' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8581" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8581).</sch:assert>
      <sch:assert id="a-1098-8582" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.25']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Observation) urn:oid:2.16.840.1.113883.11.20.9.25 STATIC 2011-09-30 (CONF:1098-8582).</sch:assert>
      <sch:assert id="a-1098-30451" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.44'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30451) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.44" (CONF:1098-30452). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32555).</sch:assert>
      <sch:assert id="a-1098-8584" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8584).</sch:assert>
      <sch:assert id="a-1098-30453" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-30453).</sch:assert>
      <sch:assert id="a-1098-31030-c" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1098-31030).</sch:assert>
      <sch:assert id="a-1098-32032" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-32032).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.44' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8577" test="@classCode='SPLY'">SHALL contain exactly one [1..1] @classCode="SPLY" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8577).</sch:assert>
      <sch:assert id="a-1098-8578" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.24']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (SubstanceAdministration/Supply) urn:oid:2.16.840.1.113883.11.20.9.24 STATIC 2011-09-30 (CONF:1098-8578).</sch:assert>
      <sch:assert id="a-1098-30463" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.43'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30463) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.43" (CONF:1098-30464). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32556).</sch:assert>
      <sch:assert id="a-1098-8580" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8580).</sch:assert>
      <sch:assert id="a-1098-30458" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-30458).</sch:assert>
      <sch:assert id="a-1098-32047" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-32047).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-errors" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.43' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-8572" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-8572).</sch:assert>
      <sch:assert id="a-1098-8573" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.24']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (SubstanceAdministration/Supply) urn:oid:2.16.840.1.113883.11.20.9.24 STATIC 2011-09-30 (CONF:1098-8573).</sch:assert>
      <sch:assert id="a-1098-30465" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.42'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30465) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.42" (CONF:1098-30466). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32557).</sch:assert>
      <sch:assert id="a-1098-8575" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-8575).</sch:assert>
      <sch:assert id="a-1098-30468-c" test="count(cda:effectiveTime[(cda:low or @value) and not(cda:low and @value)])=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-30468) such that it</sch:assert>
      <sch:assert id="a-1098-32082" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:1098-32082).</sch:assert>
      <sch:assert id="a-1098-32083" test="cda:consumable[count(cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23' and @extension='2014-06-09']])=1]">This consumable SHALL contain exactly one [1..1] Medication Information (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.23:2014-06-09) (CONF:1098-32083).</sch:assert>
      <sch:assert id="a-1098-32085" test="not(cda:precondition) or cda:precondition[@typeCode='PRCN']">The precondition, if present, SHALL contain exactly one [1..1] @typeCode="PRCN" Precondition (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:1098-32085).</sch:assert>
      <sch:assert id="a-1098-32086" test="not(cda:precondition) or cda:precondition[count(cda:criterion[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.25' and @extension='2014-06-09']])=1]">The precondition, if present, SHALL contain exactly one [1..1] Precondition for Substance Administration (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.25:2014-06-09) (CONF:1098-32086).</sch:assert>
      <sch:assert id="a-1098-32087" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-32087).</sch:assert>
      <sch:assert id="a-1098-32088" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-32088).</sch:assert>
      <sch:assert id="a-1098-32946-c" test="count(cda:effectiveTime[@operator='A'])=0 or count(cda:effectiveTime[@operator='A' and (@xsi:type='PIVL_TS' or @xsi:type='EIVL_TS')])=1">**SHALL** contain exactly one [1..1] @xsi:type="PIVL_TS" or "EIVL_TS" (CONF:1098-32946).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.42' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-errors-abstract" abstract="true">
      <sch:assert id="a-1098-32947-branch-30468-c" test="not(tested_here)">This effectiveTime **SHALL** contain either a low or a @value but not both (CONF:1098-32947).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.42' and @extension='2014-06-09']]/cda:effectiveTime[@xsi:type='IVL_TS']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-9057" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-9057).</sch:assert>
      <sch:assert id="a-1098-9072" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-9072).</sch:assert>
      <sch:assert id="a-1098-19143" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-19143).</sch:assert>
      <sch:assert id="a-1098-19144" test="cda:code[@code='11323-3']">This code SHALL contain exactly one [1..1] @code="11323-3" Health status (CONF:1098-19144).</sch:assert>
      <sch:assert id="a-1098-9074" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-9074).</sch:assert>
      <sch:assert id="a-1098-19103" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19103).</sch:assert>
      <sch:assert id="a-1098-9075" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet HealthStatus urn:oid:2.16.840.1.113883.1.11.20.12 DYNAMIC (CONF:1098-9075).</sch:assert>
      <sch:assert id="a-1098-32161" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-32161).</sch:assert>
      <sch:assert id="a-1098-32486" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-32486).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.5' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-16756" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.5'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-16756) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.5" (CONF:1098-16757). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32558).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7480" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7480).</sch:assert>
      <sch:assert id="a-1098-7481" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7481).</sch:assert>
      <sch:assert id="a-1098-7482" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.19'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7482) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.19" (CONF:1098-10502). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32570).</sch:assert>
      <sch:assert id="a-1098-7483" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7483).</sch:assert>
      <sch:assert id="a-1098-7487" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7487).</sch:assert>
      <sch:assert id="a-1098-19105" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19105).</sch:assert>
      <sch:assert id="a-1098-31229" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which MAY be selected from ValueSet Problem Type (SNOMEDCT) urn:oid:2.16.840.1.113883.3.88.12.3221.7.2 DYNAMIC (CONF:1098-31229).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.19' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.141-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-errors-abstract" abstract="true">
      <sch:assert id="a-1098-30832" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-30832).</sch:assert>
      <sch:assert id="a-1098-30833" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-30833).</sch:assert>
      <sch:assert id="a-1098-30836" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-30836).</sch:assert>
      <sch:assert id="a-1098-30838" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:1098-30838).</sch:assert>
      <sch:assert id="a-1098-31668" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31668).</sch:assert>
      <sch:assert id="a-1098-31669" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-31669).</sch:assert>
      <sch:assert id="a-1098-31670" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-31670).</sch:assert>
      <sch:assert id="a-1098-31672" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHALL contain at least one [1..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31672).</sch:assert>
      <sch:assert id="a-1098-31673" test="count(cda:participant[@typeCode='IRCP']) &gt; 0">SHALL contain at least one [1..*] participant (CONF:1098-31673) such that it SHALL contain exactly one [1..1] @typeCode="IRCP" Information Recipient (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110) (CONF:1098-31674).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.141']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-errors-abstract" />
      <sch:assert id="a-1098-30834" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.141'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30834) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.141" (CONF:1098-30835).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31675-branch-31673" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:1098-31675).</sch:assert>
      <sch:assert id="a-1098-32392-branch-31673" test="cda:participantRole[count(cda:addr) &gt; 0]">This participantRole SHALL contain at least one [1..*] addr (CONF:1098-32392).</sch:assert>
      <sch:assert id="a-1098-32422-branch-31673" test="cda:participantRole[count(cda:id) &gt; 0]">This participantRole SHALL contain at least one [1..*] id (CONF:1098-32422).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.141']]/cda:participant[@typeCode='IRCP']">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-14806" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-14806).</sch:assert>
      <sch:assert id="a-1098-14807" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-14807).</sch:assert>
      <sch:assert id="a-1098-19170" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-19170).</sch:assert>
      <sch:assert id="a-1098-14809" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-14809).</sch:assert>
      <sch:assert id="a-1098-19116" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19116).</sch:assert>
      <sch:assert id="a-1098-14810" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD" (CONF:1098-14810).</sch:assert>
      <sch:assert id="a-1098-14817" test="cda:value[@xsi:type='CD'][@code]">This value SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Smoking Status urn:oid:2.16.840.1.113883.11.20.9.38 DYNAMIC (CONF:1098-14817).</sch:assert>
      <sch:assert id="a-1098-31039" test="cda:code[@code='72166-2']">This code SHALL contain exactly one [1..1] @code="72166-2" Tobacco smoking status NHIS (CONF:1098-31039).</sch:assert>
      <sch:assert id="a-1098-31928" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-31928).</sch:assert>
      <sch:assert id="a-1098-32401" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-32401).</sch:assert>
      <sch:assert id="a-1098-32894" test="cda:effectiveTime[count(cda:low)=0]">This effectiveTime SHALL NOT contain [0..0] low (CONF:1098-32894).</sch:assert>
      <sch:assert id="a-1098-32895" test="cda:effectiveTime[count(cda:width)=0]">This effectiveTime SHALL NOT contain [0..0] width (CONF:1098-32895).</sch:assert>
      <sch:assert id="a-1098-32896" test="cda:effectiveTime[count(cda:high)=0]">This effectiveTime SHALL NOT contain [0..0] high (CONF:1098-32896).</sch:assert>
      <sch:assert id="a-1098-32897" test="cda:effectiveTime[count(cda:center)=0]">This effectiveTime SHALL NOT contain [0..0] center (CONF:1098-32897).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.78' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-14815" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.78'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-14815) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.78" (CONF:1098-14816). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32573).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7297" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7297).</sch:assert>
      <sch:assert id="a-1098-7298" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7298).</sch:assert>
      <sch:assert id="a-1098-7300" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7300).</sch:assert>
      <sch:assert id="a-1098-7301" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-7301).</sch:assert>
      <sch:assert id="a-1098-7303" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7303).</sch:assert>
      <sch:assert id="a-1098-19119" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19119).</sch:assert>
      <sch:assert id="a-1098-7304" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-7304).</sch:assert>
      <sch:assert id="a-1098-7305" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:1098-7305).</sch:assert>
      <sch:assert id="a-1098-31579" test="cda:value[@xsi:type='PQ'][@unit]">This value SHALL contain exactly one [1..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-31579).</sch:assert>
      <sch:assert id="a-1098-32886" test="not(cda:interpretationCode) or cda:interpretationCode[@code]">The interpretationCode, if present, SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Observation Interpretation (HL7) urn:oid:2.16.840.1.113883.1.11.78 DYNAMIC (CONF:1098-32886).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.27' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7299" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.27'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7299) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.27" (CONF:1098-10527). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32574).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7345" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7345).</sch:assert>
      <sch:assert id="a-1098-7346" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7346).</sch:assert>
      <sch:assert id="a-1098-19168" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-19168).</sch:assert>
      <sch:assert id="a-1098-19169" test="cda:code[@code='SEV']">This code SHALL contain exactly one [1..1] @code="SEV" Severity (CONF:1098-19169).</sch:assert>
      <sch:assert id="a-1098-7352" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7352).</sch:assert>
      <sch:assert id="a-1098-19115" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19115).</sch:assert>
      <sch:assert id="a-1098-7356" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Severity urn:oid:2.16.840.1.113883.3.88.12.3221.6.8 DYNAMIC (CONF:1098-7356).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.8' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7347" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.8'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7347) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.8" (CONF:1098-10525). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32577).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7427" test="@classCode='SPLY'">SHALL contain exactly one [1..1] @classCode="SPLY" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7427).</sch:assert>
      <sch:assert id="a-1098-7428" test="@moodCode='INT'">SHALL contain exactly one [1..1] @moodCode="INT" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7428).</sch:assert>
      <sch:assert id="a-1098-7430" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7430).</sch:assert>
      <sch:assert id="a-1098-7432" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7432).</sch:assert>
      <sch:assert id="a-1098-7444" test="not(cda:entryRelationship) or cda:entryRelationship[@typeCode='SUBJ']">The entryRelationship, if present, SHALL contain exactly one [1..1] @typeCode="SUBJ" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1098-7444).</sch:assert>
      <sch:assert id="a-1098-7445" test="not(cda:entryRelationship) or cda:entryRelationship[@inversionInd='true']">The entryRelationship, if present, SHALL contain exactly one [1..1] @inversionInd="true" True (CONF:1098-7445).</sch:assert>
      <sch:assert id="a-1098-31391" test="not(cda:entryRelationship) or cda:entryRelationship[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.20' and @extension='2014-06-09']])=1]">The entryRelationship, if present, SHALL contain exactly one [1..1] Instruction (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.20:2014-06-09) (CONF:1098-31391).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-errors" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.17' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7429" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.17'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7429) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.17" (CONF:1098-10507). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32578).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7408" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:1098-7408).</sch:assert>
      <sch:assert id="a-1098-7411" test="count(cda:manufacturedMaterial)=1">SHALL contain exactly one [1..1] manufacturedMaterial (CONF:1098-7411).</sch:assert>
      <sch:assert id="a-1098-7412" test="cda:manufacturedMaterial[count(cda:code)=1]">This manufacturedMaterial SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Medication Clinical Drug urn:oid:2.16.840.1.113762.1.4.1010.4 DYNAMIC (CONF:1098-7412).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7409" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7409) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.23" (CONF:1098-10506). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32579).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7451" test="@classCode='SPLY'">SHALL contain exactly one [1..1] @classCode="SPLY" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7451).</sch:assert>
      <sch:assert id="a-1098-7452" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7452).</sch:assert>
      <sch:assert id="a-1098-7454" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-7454).</sch:assert>
      <sch:assert id="a-1098-7455" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7455).</sch:assert>
      <sch:assert id="a-1098-7467" test="not(cda:performer) or cda:performer[count(cda:assignedEntity)=1]">The performer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1098-7467).</sch:assert>
      <sch:assert id="a-1098-10565-c" test="not(tested)">The content of addr **SHALL** be a conformant US Realm Address (AD.US.FIELDED) (2.16.840.1.113883.10.20.22.5.2) (CONF:1098-10565).</sch:assert>
      <sch:assert id="a-1098-9333-c" test="cda:product/cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23'][@extension='2014-06-09'] or cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54.2'][@extension='2014-06-09']]">A supply act  **SHALL** contain one product/Medication Information *OR* one product/Immunization Medication Information template (CONF:1098-9333).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-errors" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.18' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7453" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.18'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7453) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.18" (CONF:1098-10505). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32580).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.143-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-errors-abstract" abstract="true">
      <sch:assert id="a-1098-30949" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-30949).</sch:assert>
      <sch:assert id="a-1098-30950" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-30950).</sch:assert>
      <sch:assert id="a-1098-30951" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.143'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-30951) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.143" (CONF:1098-30952).</sch:assert>
      <sch:assert id="a-1098-30953" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-30953).</sch:assert>
      <sch:assert id="a-1098-30954" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-30954).</sch:assert>
      <sch:assert id="a-1098-30955" test="cda:code[@code='225773000']">This code SHALL contain exactly one [1..1] @code="225773000" Preference (CONF:1098-30955).</sch:assert>
      <sch:assert id="a-1098-30956" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:1098-30956).</sch:assert>
      <sch:assert id="a-1098-30957" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Priority Level urn:oid:2.16.840.1.113883.11.20.9.60 DYNAMIC (CONF:1098-30957).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.143']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-16558" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-16558).</sch:assert>
      <sch:assert id="a-1098-16559" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-16559).</sch:assert>
      <sch:assert id="a-1098-19174" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-19174).</sch:assert>
      <sch:assert id="a-1098-19175" test="cda:code[@code='11367-0']">This code SHALL contain exactly one [1..1] @code="11367-0" History of tobacco use (CONF:1098-19175).</sch:assert>
      <sch:assert id="a-1098-16561" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-16561).</sch:assert>
      <sch:assert id="a-1098-19118" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19118).</sch:assert>
      <sch:assert id="a-1098-16564" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-16564).</sch:assert>
      <sch:assert id="a-1098-16565" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:1098-16565).</sch:assert>
      <sch:assert id="a-1098-16562" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Tobacco Use urn:oid:2.16.840.1.113883.11.20.9.41 DYNAMIC (CONF:1098-16562).</sch:assert>
      <sch:assert id="a-1098-32172" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1098-32172).</sch:assert>
      <sch:assert id="a-1098-32400" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-32400).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.85' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-16566" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.85'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-16566) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.85" (CONF:1098-16567). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32589).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7391" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1098-7391).</sch:assert>
      <sch:assert id="a-1098-7392" test="@moodCode='INT'">SHALL contain exactly one [1..1] @moodCode="INT" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1098-7392).</sch:assert>
      <sch:assert id="a-1098-16884" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Patient Education urn:oid:2.16.840.1.113883.11.20.9.34 DYNAMIC (CONF:1098-16884).</sch:assert>
      <sch:assert id="a-1098-7396" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-7396).</sch:assert>
      <sch:assert id="a-1098-19106" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1098-19106).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.20' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7393" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.20'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7393) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.20" (CONF:1098-10503). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32598).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.119-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31471" test="count(cda:time)=1">SHALL contain exactly one [1..1] time (CONF:1098-31471).</sch:assert>
      <sch:assert id="a-1098-31472" test="count(cda:assignedAuthor)=1">SHALL contain exactly one [1..1] assignedAuthor (CONF:1098-31472).</sch:assert>
      <sch:assert id="a-1098-31473" test="cda:assignedAuthor[count(cda:id) &gt; 0]">This assignedAuthor SHALL contain at least one [1..*] id (CONF:1098-31473).</sch:assert>
      <sch:assert id="a-1098-32017" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-32017) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.119" (CONF:1098-32018).</sch:assert>
      <sch:assert id="a-1098-32628-c" test="not(tested)">If the ID isn't referencing an author described elsewhere in the document, then the author components required in US Realm Header are required here as well (CONF:1098-32628).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-errors" context="cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.122-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31485" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-31485).</sch:assert>
      <sch:assert id="a-1098-31486" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-31486).</sch:assert>
      <sch:assert id="a-1098-31487" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.122'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-31487) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.122" (CONF:1098-31488).</sch:assert>
      <sch:assert id="a-1098-31489" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-31489).</sch:assert>
      <sch:assert id="a-1098-31490" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31490).</sch:assert>
      <sch:assert id="a-1098-31491" test="cda:code[@nullFlavor='NP']">This code SHALL contain exactly one [1..1] @nullFlavor="NP" Not Present (CodeSystem: HL7NullFlavor urn:oid:2.16.840.1.113883.5.1008) (CONF:1098-31491).</sch:assert>
      <sch:assert id="a-1098-31498" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31498).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.122']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.118-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31500" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-31500).</sch:assert>
      <sch:assert id="a-1098-31501" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-31501).</sch:assert>
      <sch:assert id="a-1098-31502" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.118'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-31502) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.118" (CONF:1098-31503).</sch:assert>
      <sch:assert id="a-1098-31504" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-31504).</sch:assert>
      <sch:assert id="a-1098-31505" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-31505).</sch:assert>
      <sch:assert id="a-1098-31506" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31506).</sch:assert>
      <sch:assert id="a-1098-31507" test="cda:code[@code='416118004']">This code SHALL contain exactly one [1..1] @code="416118004" Administration (CONF:1098-31507).</sch:assert>
      <sch:assert id="a-1098-31508" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:1098-31508).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.118']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-9002" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:1098-9002).</sch:assert>
      <sch:assert id="a-1098-9006" test="count(cda:manufacturedMaterial)=1">SHALL contain exactly one [1..1] manufacturedMaterial (CONF:1098-9006).</sch:assert>
      <sch:assert id="a-1098-9007" test="cda:manufacturedMaterial[count(cda:code)=1]">This manufacturedMaterial SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet CVX Vaccines Administered Vaccine Set urn:oid:2.16.840.1.113762.1.4.1010.6 DYNAMIC (CONF:1098-9007).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-9004" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-9004) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.54" (CONF:1098-10499). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32602).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-7369" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Problem urn:oid:2.16.840.1.113883.3.88.12.3221.7.4 DYNAMIC (CONF:1098-7369).</sch:assert>
      <sch:assert id="a-1098-32396" test="count(cda:code)=1">SHALL contain exactly one [1..1] code with @xsi:type="CD" (CONF:1098-32396).</sch:assert>
      <sch:assert id="a-1098-32397" test="cda:code[@code='ASSERTION']">This code SHALL contain exactly one [1..1] @code="ASSERTION" Assertion (CONF:1098-32397).</sch:assert>
      <sch:assert id="a-1098-32398" test="cda:code[@codeSystem='2.16.840.1.113883.5.4']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.5.4" (CodeSystem: HL7ActCode urn:oid:2.16.840.1.113883.5.4) (CONF:1098-32398).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-errors" context="cda:criterion[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.25' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-errors-abstract" />
      <sch:assert id="a-1098-7372" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.25'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-7372) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.25" (CONF:1098-10517). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32603).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31931" test="@classCode='DOCCLIN'">SHALL contain exactly one [1..1] @classCode="DOCCLIN" Clinical Document (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-31931).</sch:assert>
      <sch:assert id="a-1098-31932" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-31932).</sch:assert>
      <sch:assert id="a-1098-31933" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31933).</sch:assert>
      <sch:assert id="a-1098-32748" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.115'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-32748) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.115" (CONF:1098-32750). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1098-32749).</sch:assert>
      <sch:assert id="a-1098-32751" test="count(cda:id)=1">SHALL contain exactly one [1..1] id (CONF:1098-32751).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-errors" context="cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.115' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.129-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31945" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" act (CodeSystem: HL7ActCode urn:oid:2.16.840.1.113883.5.4) (CONF:1098-31945).</sch:assert>
      <sch:assert id="a-1098-31946" test="@moodCode='INT'">SHALL contain exactly one [1..1] @moodCode="INT" Intent (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-31946).</sch:assert>
      <sch:assert id="a-1098-31947" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.129'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-31947) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.129" (CONF:1098-31948).</sch:assert>
      <sch:assert id="a-1098-31950" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-31950).</sch:assert>
      <sch:assert id="a-1098-31951" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-31951).</sch:assert>
      <sch:assert id="a-1098-31954" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-31954).</sch:assert>
      <sch:assert id="a-1098-31967" test="count(cda:entryRelationship[@typeCode='COMP'])=1">SHALL contain exactly one [1..1] entryRelationship (CONF:1098-31967) such that it SHALL contain exactly one [1..1] @typeCode="COMP" has component (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:1098-31968).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.129']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-31967-branch-31967-errors-abstract" abstract="true">
      <sch:assert id="a-1098-31969-branch-31967" test="count(cda:act)=1">SHALL contain exactly one [1..1] act (CONF:1098-31969). This act SHALL contain at least one [1..*] id (CONF:1098-31972). This act SHALL contain exactly one [1..1] statusCode (CONF:1098-31974). This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-31975).</sch:assert>
      <sch:assert id="a-1098-31970-branch-31967" test="cda:act[@classCode='ACT']">This act SHALL contain exactly one [1..1] @classCode="ACT" ACT (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1098-31970).</sch:assert>
      <sch:assert id="a-1098-31971-branch-31967" test="cda:act[@moodCode='INT']">This act SHALL contain exactly one [1..1] @moodCode="INT" intent (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1098-31971).</sch:assert>
      <sch:assert id="a-1098-31973-branch-31967" test="cda:act[count(cda:code)=1]">This act SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Payer urn:oid:2.16.840.1.114222.4.11.3591 DYNAMIC (CONF:1098-31973).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-31967-branch-31967-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.129']]/cda:entryRelationship[@typeCode='COMP']">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-31967-branch-31967-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.120-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-errors-abstract" abstract="true">
      <sch:assert id="a-1098-32091" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CONF:1098-32091).</sch:assert>
      <sch:assert id="a-1098-32097" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.24']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (SubstanceAdministration/Supply) urn:oid:2.16.840.1.113883.11.20.9.24 STATIC 2014-09-01 (CONF:1098-32097).</sch:assert>
      <sch:assert id="a-1098-32098" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.120'])=1">SHALL contain exactly one [1..1] templateId (CONF:1098-32098) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.120" (CONF:1098-32099).</sch:assert>
      <sch:assert id="a-1098-32100" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1098-32100).</sch:assert>
      <sch:assert id="a-1098-32101" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1098-32101).</sch:assert>
      <sch:assert id="a-1098-32102" test="cda:statusCode[@code='active']">This statusCode SHALL contain exactly one [1..1] @code="active" Active (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1098-32102).</sch:assert>
      <sch:assert id="a-1098-32103" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1098-32103).</sch:assert>
      <sch:assert id="a-1098-32131" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:1098-32131).</sch:assert>
      <sch:assert id="a-1098-32132" test="cda:consumable[count(cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54' and @extension='2014-06-09']])=1]">This consumable SHALL contain exactly one [1..1] Immunization Medication Information (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.54:2014-06-09) (CONF:1098-32132).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.120']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-errors-abstract" abstract="true">
      <sch:assert id="a-1133-21962" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.30.3.34'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:1133-21962) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.30.3.34" (CONF:1133-21963). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:1133-27848).</sch:assert>
      <sch:assert id="a-1133-21964" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1133-21964).</sch:assert>
      <sch:assert id="a-1133-21966" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1133-21966).</sch:assert>
      <sch:assert id="a-1133-21960" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:1133-21960).</sch:assert>
      <sch:assert id="a-1133-21961" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:1133-21961).</sch:assert>
      <sch:assert id="a-1133-21965" test="cda:code[@code='8665-2']">This code SHALL contain exactly one [1..1] @code="8665-2" Last menstrual period start date (CONF:1133-21965).</sch:assert>
      <sch:assert id="a-1133-23237" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1133-23237).</sch:assert>
      <sch:assert id="a-1133-21967" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:1133-21967).</sch:assert>
      <sch:assert id="a-1133-21968" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:1133-21968).</sch:assert>
      <sch:assert id="a-1133-22065" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1133-22065).</sch:assert>
      <sch:assert id="a-1133-22066" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1133-22066).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.30.3.34' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-7285" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.27' and @extension='2014-06-09']])=1]) &gt; 0">SHALL contain at least one [1..*] component (CONF:1198-7285) such that it SHALL contain exactly one [1..1] Vital Sign Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.27:2014-06-09) (CONF:1198-15946).</sch:assert>
      <sch:assert id="a-1198-7284" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-7284).</sch:assert>
      <sch:assert id="a-1198-32740" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-32740).</sch:assert>
      <sch:assert id="a-1198-7279" test="@classCode='CLUSTER'">SHALL contain exactly one [1..1] @classCode="CLUSTER" CLUSTER (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7279).</sch:assert>
      <sch:assert id="a-1198-7280" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7280).</sch:assert>
      <sch:assert id="a-1198-7282" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-7282).</sch:assert>
      <sch:assert id="a-1198-19120" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1198-19120).</sch:assert>
      <sch:assert id="a-1198-7288" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-7288).</sch:assert>
      <sch:assert id="a-1198-32741" test="cda:code[@code='46680005']">This code SHALL contain exactly one [1..1] @code="46680005" Vital Signs (CONF:1198-32741).</sch:assert>
      <sch:assert id="a-1198-32742" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" SNOMED CT (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:1198-32742).</sch:assert>
      <sch:assert id="a-1198-32743-c" test="not(tested)">This code SHALL contain exactly one [1..1] translation (CONF:1198-32743) such that it SHALL contain exactly one [1..1] @code="74728-7" Vital signs, weight, height, head circumference, oximetry, BMI, and BSA panel - HL7.CCDAr1.1 (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32744). SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32746).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7281" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7281) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.26" (CONF:1198-10528). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32582).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-32743-branch-32743-errors-abstract" abstract="true">
      <sch:assert id="a-1198-32744-branch-32743-c" test="not(tested)">SHALL contain exactly one [1..1] @code="74728-7" Vital signs, weight, height, head circumference, oximetry, BMI, and BSA panel - HL7.CCDAr1.1 (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32744).</sch:assert>
      <sch:assert id="a-1198-32746-branch-32743-c" test="not(tested)">SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32746).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-32743-branch-32743-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26' and @extension='2015-08-01']]/cda:code/cda:translation[@code='74728-7'][@codeSystem='2.16.840.1.113883.6.1']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-32743-branch-32743-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15479" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15479).</sch:assert>
      <sch:assert id="a-1198-15481" test="not(cda:entry) or cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.34' and @extension='2015-08-01']])=1]">The entry, if present, SHALL contain exactly one [1..1] Hospital Admission Diagnosis (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.34:2015-08-01) (CONF:1198-15481).</sch:assert>
      <sch:assert id="a-1198-15480" test="cda:code[@code='46241-6']">This code SHALL contain exactly one [1..1] @code="46241-6" Hospital Admission diagnosis (CONF:1198-15480).</sch:assert>
      <sch:assert id="a-1198-30865" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30865).</sch:assert>
      <sch:assert id="a-1198-9932" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-9932).</sch:assert>
      <sch:assert id="a-1198-9933" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-9933).</sch:assert>
      <sch:assert id="a-1198-32749" test="cda:code[count(cda:translation[@code='42347-5'][@codeSystem='2.16.840.1.113883.6.1'])=1]">This code SHALL contain exactly one [1..1] translation (CONF:1198-32749) such that it SHALL contain exactly one [1..1] @code="42347-5" Admission Diagnosis (CONF:1198-32750). SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:1198-32751).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.43' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-9930" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.43'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-9930) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.43" (CONF:1198-10391). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32563).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-8847" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:1198-8847).</sch:assert>
      <sch:assert id="a-1198-8833" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-8833).</sch:assert>
      <sch:assert id="a-1198-15546" test="cda:consumable[count(cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54' and @extension='2014-06-09']])=1]">This consumable SHALL contain exactly one [1..1] Immunization Medication Information (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.54:2014-06-09) (CONF:1198-15546).</sch:assert>
      <sch:assert id="a-1198-8826" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-8826).</sch:assert>
      <sch:assert id="a-1198-8827" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.18']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet MoodCodeEvnInt urn:oid:2.16.840.1.113883.11.20.9.18 STATIC (CONF:1198-8827).</sch:assert>
      <sch:assert id="a-1198-8985" test="@negationInd">SHALL contain exactly one [1..1] @negationInd (CONF:1198-8985).</sch:assert>
      <sch:assert id="a-1198-8829" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-8829).</sch:assert>
      <sch:assert id="a-1198-8834" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-8834).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.52' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-8828" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.52'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-8828) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.52" (CONF:1198-10498). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32528).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-7133" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1198-7133).</sch:assert>
      <sch:assert id="a-1198-7134" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-7134).</sch:assert>
      <sch:assert id="a-1198-7143" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:1198-7143).</sch:assert>
      <sch:assert id="a-1198-7151" test="not(cda:referenceRange) or cda:referenceRange[count(cda:observationRange)=1]">The referenceRange, if present, SHALL contain exactly one [1..1] observationRange (CONF:1198-7151).</sch:assert>
      <sch:assert id="a-1198-7130" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7130).</sch:assert>
      <sch:assert id="a-1198-7131" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7131).</sch:assert>
      <sch:assert id="a-1198-7137" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-7137).</sch:assert>
      <sch:assert id="a-1198-14849" test="cda:statusCode[@code and @code=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.39']/voc:code/@value]">This statusCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Result Status urn:oid:2.16.840.1.113883.11.20.9.39 STATIC (CONF:1198-14849).</sch:assert>
      <sch:assert id="a-1198-7140" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-7140).</sch:assert>
      <sch:assert id="a-1198-31484-c" test="not(tested)">If Observation/value is a physical quantity (**xsi:type="PQ"**), the unit of measure **SHALL** be selected from ValueSet UnitsOfMeasureCaseSensitive 2.16.840.1.113883.1.11.12839 **DYNAMIC** (CONF:1198-31484).</sch:assert>
      <sch:assert id="a-1198-32476" test="not(cda:interpretationCode) or cda:interpretationCode[@code]">The interpretationCode, if present, SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Observation Interpretation (HL7) urn:oid:2.16.840.1.113883.1.11.78 DYNAMIC (CONF:1198-32476).</sch:assert>
      <sch:assert id="a-1198-7152-c" test="not(cda:referenceRange/cda:observationRange/cda:code)">This observationRange SHALL NOT contain [0..0] code (CONF:1198-7152).</sch:assert>
      <sch:assert id="a-1198-32175" test="not(cda:referenceRange/cda:observationRange) or cda:referenceRange/cda:observationRange[count(cda:value)=1]">This observationRange SHALL contain exactly one [1..1] value (CONF:1198-32175).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.2' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7136" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.2'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7136) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.2" (CONF:1198-9138). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32575).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors-abstract" abstract="true">
      <sch:assert id="a-81-32754" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:81-32754).</sch:assert>
      <sch:assert id="a-81-32755" test="cda:text[count(cda:reference)=1]">This text SHALL contain exactly one [1..1] reference (CONF:81-32755).</sch:assert>
      <sch:assert id="a-81-32770" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:81-32770).</sch:assert>
      <sch:assert id="a-81-32771" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.18']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet MoodCodeEvnInt urn:oid:2.16.840.1.113883.11.20.9.18 STATIC 2011-04-03 (CONF:81-32771).</sch:assert>
      <sch:assert id="a-81-32774-c" test="count(cda:text/cda:reference[@value])=0 or starts-with(cda:text/cda:reference/@value, '#')">This reference/@value SHALL begin with a '#' and SHALL point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:81-32774).</sch:assert>
      <sch:assert id="a-81-32775" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:81-32775).</sch:assert>
      <sch:assert id="a-81-32776" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:81-32776).</sch:assert>
      <sch:assert id="a-81-32777" test="cda:consumable[count(cda:manufacturedProduct)=1]">This consumable SHALL contain exactly one [1..1] manufacturedProduct (CONF:81-32777).</sch:assert>
      <sch:assert id="a-81-32778" test="cda:consumable/cda:manufacturedProduct[count(cda:manufacturedLabeledDrug)=1]">This manufacturedProduct SHALL contain exactly one [1..1] manufacturedLabeledDrug (CONF:81-32778).</sch:assert>
      <sch:assert id="a-81-32779" test="cda:consumable/cda:manufacturedProduct/cda:manufacturedLabeledDrug[@nullFlavor='NA']">This manufacturedLabeledDrug SHALL contain exactly one [1..1] @nullFlavor="NA" Not Applicable (CONF:81-32779).</sch:assert>
      <sch:assert id="a-81-32780" test="cda:code[@code='76662-6']">This code SHALL contain exactly one [1..1] @code="76662-6" Instructions Medication (CONF:81-32780).</sch:assert>
      <sch:assert id="a-81-32781" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:81-32781).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.147']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors-abstract" />
      <sch:assert id="a-81-32753" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.147'])=1">SHALL contain exactly one [1..1] templateId (CONF:81-32753) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.147" (CONF:81-32772).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-CLOSEDTEMPLATE">
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors-CL-abstract" abstract="true">
      <sch:assert id="a-81-5432-CL" test="count(.//cda:templateId[@root != '2.16.840.1.113883.10.20.22.4.147'])=0">'urn:oid:2.16.840.1.113883.10.20.22.4.147' is a closed template, only defined templates are allowed.</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors-CL" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.147']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-errors-CL-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors">
    <!--Pattern is used in an implied relationship.-->
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15367" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15367).</sch:assert>
      <sch:assert id="a-1198-15368" test="cda:code[@code='11369-6']">This code SHALL contain exactly one [1..1] @code="11369-6" Immunizations (CONF:1198-15368).</sch:assert>
      <sch:assert id="a-1198-7967" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7967).</sch:assert>
      <sch:assert id="a-1198-7968" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7968).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7965" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7965) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.2" (CONF:1198-10399). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32529).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-9019-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.52'][@extension='2015-08-01']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1198-9019) such that it SHALL contain exactly one [1..1] Immunization Activity (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.52:2015-08-01) (CONF:1198-15495).</sch:assert>
      <sch:assert id="a-1198-15369" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15369).</sch:assert>
      <sch:assert id="a-1198-15370" test="cda:code[@code='11369-6']">This code SHALL contain exactly one [1..1] @code="11369-6" Immunizations (CONF:1198-15370).</sch:assert>
      <sch:assert id="a-1198-9017" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-9017).</sch:assert>
      <sch:assert id="a-1198-9018" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-9018).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-9015" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-9015) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.2.1" (CONF:1198-10400). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32530).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15355" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15355).</sch:assert>
      <sch:assert id="a-1198-15489" test="not(cda:entry) or cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.33' and @extension='2015-08-01']])=1]">The entry, if present, SHALL contain exactly one [1..1] Hospital Discharge Diagnosis (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.33:2015-08-01) (CONF:1198-15489).</sch:assert>
      <sch:assert id="a-1198-15356" test="cda:code[@code='11535-2' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="11535-2" Hospital Discharge Diagnosis (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:1198-15356).</sch:assert>
      <sch:assert id="a-1198-30861" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30861).</sch:assert>
      <sch:assert id="a-1198-7981" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7981).</sch:assert>
      <sch:assert id="a-1198-7982" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7982).</sch:assert>
      <sch:assert id="a-1198-32834" test="cda:code[count(cda:translation[@code='78375-3'][@codeSystem='2.16.840.1.113883.6.1'])=1]">This code SHALL contain exactly one [1..1] translation (CONF:1198-32834) such that it SHALL contain exactly one [1..1] @code="78375-3" Discharge Diagnosis (CONF:1198-32835). SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32836).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.24' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7979" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.24'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7979) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.24" (CONF:1198-10394). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32549).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-14926" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-14926) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.4" (CONF:1198-14927). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32508).</sch:assert>
      <sch:assert id="a-1198-9049" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-9049).</sch:assert>
      <sch:assert id="a-1198-9050" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-9050).</sch:assert>
      <sch:assert id="a-1198-9058-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Problem urn:oid:2.16.840.1.113883.3.88.12.3221.7.4 DYNAMIC (CONF:1198-9058).</sch:assert>
      <sch:assert id="a-1198-9041" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-9041).</sch:assert>
      <sch:assert id="a-1198-9042" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-9042).</sch:assert>
      <sch:assert id="a-1198-9043" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-9043).</sch:assert>
      <sch:assert id="a-1198-9045" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Problem Type (SNOMEDCT) urn:oid:2.16.840.1.113883.3.88.12.3221.7.2 DYNAMIC (CONF:1198-9045).</sch:assert>
      <sch:assert id="a-1198-19112" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1198-19112).</sch:assert>
      <sch:assert id="a-1198-15603" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:1198-15603).</sch:assert>
      <sch:assert id="a-1198-32950-c" test="not(tested)">If code is selected from ValueSet Problem Type (SNOMEDCT) urn:oid:2.16.840.1.113883.3.88.12.3221.7.2 **DYNAMIC**, then it **SHALL** have at least one [1..*] translation, which **SHOULD** be selected from ValueSet Problem Type (LOINC) urn:oid:2.16.840.1.113762.1.4.1099.28 **DYNAMIC** (CONF:1198-32950) (CONF:1198-32950).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-8550" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.38'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-8550) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.38" (CONF:1198-10526). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32495).</sch:assert>
      <sch:assert id="a-1198-8558" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Social History Type urn:oid:2.16.840.1.113883.3.88.12.80.60 DYNAMIC (CONF:1198-8558).</sch:assert>
      <sch:assert id="a-1198-8553" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-8553).</sch:assert>
      <sch:assert id="a-1198-8548" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-8548).</sch:assert>
      <sch:assert id="a-1198-8549" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-8549).</sch:assert>
      <sch:assert id="a-1198-8551" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-8551).</sch:assert>
      <sch:assert id="a-1198-19117" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1198-19117).</sch:assert>
      <sch:assert id="a-1198-8555-c" test="not(tested)">If Observation/value is a physical quantity (xsi:type="PQ"), the unit of measure **SHALL** be selected from ValueSet UnitsOfMeasureCaseSensitive (2.16.840.1.113883.1.11.12839) *DYNAMIC* (CONF:1198-8555).</sch:assert>
      <sch:assert id="a-1198-31868" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-31868).</sch:assert>
      <sch:assert id="a-1198-32951-c" test="cda:code/@codeSystem='2.16.840.1.113883.6.1' or cda:code/cda:translation">If @codeSystem is not LOINC, then this code **SHALL** contain at least one [1..*] translation, which **SHOULD** be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32951).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.38' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15431" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15431).</sch:assert>
      <sch:assert id="a-1198-15432" test="cda:code[@code='30954-2']">This code SHALL contain exactly one [1..1] @code="30954-2" Relevant diagnostic tests and/or laboratory data (CONF:1198-15432).</sch:assert>
      <sch:assert id="a-1198-31041" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-31041).</sch:assert>
      <sch:assert id="a-1198-8891" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-8891).</sch:assert>
      <sch:assert id="a-1198-7118" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7118).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7116" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7116) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.3" (CONF:1198-9136). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32591).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-7124" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.2' and @extension='2015-08-01']])=1]) &gt; 0">SHALL contain at least one [1..*] component (CONF:1198-7124) such that it SHALL contain exactly one [1..1] Result Observation (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.2:2015-08-01) (CONF:1198-14850).</sch:assert>
      <sch:assert id="a-1198-7128" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-7128).</sch:assert>
      <sch:assert id="a-1198-7123" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-7123).</sch:assert>
      <sch:assert id="a-1198-7121" test="@classCode">SHALL contain exactly one [1..1] @classCode (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7121).</sch:assert>
      <sch:assert id="a-1198-7122" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7122).</sch:assert>
      <sch:assert id="a-1198-7127" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-7127).</sch:assert>
      <sch:assert id="a-1198-14848" test="cda:statusCode[@code and @code=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.39']/voc:code/@value]">This statusCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Result Status urn:oid:2.16.840.1.113883.11.20.9.39 STATIC (CONF:1198-14848).</sch:assert>
      <sch:assert id="a-1198-32488" test="not(cda:effectiveTime) or cda:effectiveTime[count(cda:low)=1]">The effectiveTime, if present, SHALL contain exactly one [1..1] low (CONF:1198-32488).</sch:assert>
      <sch:assert id="a-1198-32489" test="not(cda:effectiveTime) or cda:effectiveTime[count(cda:high)=1]">The effectiveTime, if present, SHALL contain exactly one [1..1] high (CONF:1198-32489).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7126" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7126) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.1" (CONF:1198-9134). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32588).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7112-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.1'][@extension='2015-08-01']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1198-7112) such that it SHALL contain exactly one [1..1] Result Organizer (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.1:2015-08-01) (CONF:1198-15516).</sch:assert>
      <sch:assert id="a-1198-15433" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15433).</sch:assert>
      <sch:assert id="a-1198-15434" test="cda:code[@code='30954-2']">This code SHALL contain exactly one [1..1] @code="30954-2" Relevant diagnostic tests and/or laboratory data (CONF:1198-15434).</sch:assert>
      <sch:assert id="a-1198-8892" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-8892).</sch:assert>
      <sch:assert id="a-1198-7111" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7111).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7108" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7108) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.3.1" (CONF:1198-9137). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32592).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15482" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15482).</sch:assert>
      <sch:assert id="a-1198-15483" test="cda:code[@code='42346-7']">This code SHALL contain exactly one [1..1] @code="42346-7" Medications on Admission (CONF:1198-15483).</sch:assert>
      <sch:assert id="a-1198-10100" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-10100).</sch:assert>
      <sch:assert id="a-1198-10101" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-10101).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.44' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-10098" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.44'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-10098) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.44" (CONF:1198-10392). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32560).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-7666" test="count(cda:entryRelationship[@typeCode='SUBJ'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:1198-7666) such that it SHALL contain exactly one [1..1] Problem Observation (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.4:2015-08-01) (CONF:1198-15536). SHALL contain exactly one [1..1] @typeCode="SUBJ" Has Subject (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1198-7667).</sch:assert>
      <sch:assert id="a-1198-19147" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-19147).</sch:assert>
      <sch:assert id="a-1198-7663" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7663).</sch:assert>
      <sch:assert id="a-1198-7664" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7664).</sch:assert>
      <sch:assert id="a-1198-19148" test="cda:code[@code='11535-2']">This code SHALL contain exactly one [1..1] @code="11535-2" Hospital discharge diagnosis (CONF:1198-19148).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.33' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-16764" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.33'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-16764) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.33" (CONF:1198-16765). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32534).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-14892" test="count(cda:entryRelationship[@typeCode='SUBJ'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:1198-14892) such that it SHALL contain exactly one [1..1] Problem Observation (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.4:2015-08-01) (CONF:1198-14898). SHALL contain exactly one [1..1] @typeCode="SUBJ" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1198-14893).</sch:assert>
      <sch:assert id="a-1198-19182" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-19182).</sch:assert>
      <sch:assert id="a-1198-14889" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-14889).</sch:assert>
      <sch:assert id="a-1198-14890" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-14890).</sch:assert>
      <sch:assert id="a-1198-19183" test="cda:code[@code='29308-4']">This code SHALL contain exactly one [1..1] @code="29308-4" Diagnosis (CONF:1198-19183).</sch:assert>
      <sch:assert id="a-1198-32160" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-32160).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.80' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-14895" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.80'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-14895) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.80" (CONF:1198-14896). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32542).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15474" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15474).</sch:assert>
      <sch:assert id="a-1198-15475" test="cda:code[@code='11348-0']">This code SHALL contain exactly one [1..1] @code="11348-0" History of Past Illness (CONF:1198-15475).</sch:assert>
      <sch:assert id="a-1198-30831" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30831).</sch:assert>
      <sch:assert id="a-1198-7830" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7830).</sch:assert>
      <sch:assert id="a-1198-7831" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7831).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.20' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7828" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.20'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7828) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.20" (CONF:1198-10390). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32536).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-7674" test="count(cda:entryRelationship[@typeCode='SUBJ'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:1198-7674) such that it SHALL contain exactly one [1..1] Problem Observation (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.4:2015-08-01) (CONF:1198-15535). SHALL contain exactly one [1..1] @typeCode="SUBJ" Has Subject (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1198-7675).</sch:assert>
      <sch:assert id="a-1198-19145" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-19145).</sch:assert>
      <sch:assert id="a-1198-7671" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7671).</sch:assert>
      <sch:assert id="a-1198-7672" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7672).</sch:assert>
      <sch:assert id="a-1198-19146" test="cda:code[@code='46241-6']">This code SHALL contain exactly one [1..1] @code="46241-6" Admission diagnosis (CONF:1198-19146).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.34' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-16747" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.34'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-16747) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.34" (CONF:1198-16748). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32535).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15242" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15242).</sch:assert>
      <sch:assert id="a-1198-15243" test="cda:code[@code='8716-3']">This code SHALL contain exactly one [1..1] @code="8716-3" Vital Signs (CONF:1198-15243).</sch:assert>
      <sch:assert id="a-1198-30902" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30902).</sch:assert>
      <sch:assert id="a-1198-9966" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-9966).</sch:assert>
      <sch:assert id="a-1198-7270" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7270).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7268" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7268) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.4" (CONF:1198-10451). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32584).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7276-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26'][@extension='2015-08-01']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1198-7276) such that it SHALL contain exactly one [1..1] Vital Signs Organizer (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.26:2015-08-01) (CONF:1198-15964).</sch:assert>
      <sch:assert id="a-1198-15962" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15962).</sch:assert>
      <sch:assert id="a-1198-15963" test="cda:code[@code='8716-3']">This code SHALL contain exactly one [1..1] @code="8716-3" Vital Signs (CONF:1198-15963).</sch:assert>
      <sch:assert id="a-1198-30903" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30903).</sch:assert>
      <sch:assert id="a-1198-9967" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-9967).</sch:assert>
      <sch:assert id="a-1198-7275" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7275).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7273" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7273) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.4.1" (CONF:1198-10452). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32585).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15407" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15407).</sch:assert>
      <sch:assert id="a-1198-15408" test="cda:code[@code='11450-4']">This code SHALL contain exactly one [1..1] @code="11450-4" Problem List (CONF:1198-15408).</sch:assert>
      <sch:assert id="a-1198-31141" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-31141).</sch:assert>
      <sch:assert id="a-1198-7879" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7879).</sch:assert>
      <sch:assert id="a-1198-7880" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7880).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7877" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7877) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.5" (CONF:1198-10440). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32511).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-9034" test="count(cda:entryRelationship[@typeCode='SUBJ'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:1198-9034) such that it SHALL contain exactly one [1..1] Problem Observation (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.4:2015-08-01) (CONF:1198-15980). SHALL contain exactly one [1..1] @typeCode="SUBJ" Has subject (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:1198-9035).</sch:assert>
      <sch:assert id="a-1198-9027" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-9027).</sch:assert>
      <sch:assert id="a-1198-9029" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-9029).</sch:assert>
      <sch:assert id="a-1198-9030" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-9030).</sch:assert>
      <sch:assert id="a-1198-9024" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-9024).</sch:assert>
      <sch:assert id="a-1198-9025" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-9025).</sch:assert>
      <sch:assert id="a-1198-9026" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-9026).</sch:assert>
      <sch:assert id="a-1198-19184" test="cda:code[@code='CONC']">This code SHALL contain exactly one [1..1] @code="CONC" Concern (CONF:1198-19184).</sch:assert>
      <sch:assert id="a-1198-9032" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:1198-9032).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.3' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-16772" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.3'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-16772) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.3" (CONF:1198-16773). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32509).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-9183-c" test="((count(@nullFlavor)=1) or (count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.3'][@extension='2015-08-01']])=1]) &gt; 0)) and  (not((count(@nullFlavor)=1) and  (count(cda:entry) &gt; 0)))">SHALL contain at least one [1..*] entry (CONF:1198-9183) such that it SHALL contain exactly one [1..1] Problem Concern Act (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.3:2015-08-01) (CONF:1198-15506).</sch:assert>
      <sch:assert id="a-1198-15409" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15409).</sch:assert>
      <sch:assert id="a-1198-15410" test="cda:code[@code='11450-4']">This code SHALL contain exactly one [1..1] @code="11450-4" Problem List (CONF:1198-15410).</sch:assert>
      <sch:assert id="a-1198-9181" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-9181).</sch:assert>
      <sch:assert id="a-1198-9182" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-9182).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-9179" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-9179) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.5.1" (CONF:1198-10441). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32510).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-14819" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-14819).</sch:assert>
      <sch:assert id="a-1198-14820" test="cda:code[@code='29762-2']">This code SHALL contain exactly one [1..1] @code="29762-2" Social History (CONF:1198-14820).</sch:assert>
      <sch:assert id="a-1198-30814" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-30814).</sch:assert>
      <sch:assert id="a-1198-7938" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7938).</sch:assert>
      <sch:assert id="a-1198-7939" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7939).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7936" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7936) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.17" (CONF:1198-10449). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32494).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-5361" test="count(cda:typeId)=1">SHALL contain exactly one [1..1] typeId (CONF:1198-5361).</sch:assert>
      <sch:assert id="a-1198-5363" test="count(cda:id)=1">SHALL contain exactly one [1..1] id (CONF:1198-5363).</sch:assert>
      <sch:assert id="a-1198-5253" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-5253).</sch:assert>
      <sch:assert id="a-1198-5266" test="count(cda:recordTarget) &gt; 0">SHALL contain at least one [1..*] recordTarget (CONF:1198-5266).</sch:assert>
      <sch:assert id="a-1198-5267" test="cda:recordTarget[count(cda:patientRole)=1]">Such recordTargets SHALL contain exactly one [1..1] patientRole (CONF:1198-5267).</sch:assert>
      <sch:assert id="a-1198-5280" test="cda:recordTarget/cda:patientRole[count(cda:telecom) &gt; 0]">This patientRole SHALL contain at least one [1..*] telecom (CONF:1198-5280).</sch:assert>
      <sch:assert id="a-1198-5283" test="cda:recordTarget/cda:patientRole[count(cda:patient)=1]">This patientRole SHALL contain exactly one [1..1] patient (CONF:1198-5283).</sch:assert>
      <sch:assert id="a-1198-5298" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:birthTime)=1]">This patient SHALL contain exactly one [1..1] birthTime (CONF:1198-5298).</sch:assert>
      <sch:assert id="a-1198-5385" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:guardianPerson)=1]">The guardian, if present, SHALL contain exactly one [1..1] guardianPerson (CONF:1198-5385).</sch:assert>
      <sch:assert id="a-1198-5396" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace) or cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace[count(cda:place)=1]">The birthplace, if present, SHALL contain exactly one [1..1] place (CONF:1198-5396).</sch:assert>
      <sch:assert id="a-1198-5397" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace/cda:place) or cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace/cda:place[count(cda:addr)=1]">This place SHALL contain exactly one [1..1] addr (CONF:1198-5397).</sch:assert>
      <sch:assert id="a-1198-5417" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization) or cda:recordTarget/cda:patientRole/cda:providerOrganization[count(cda:id) &gt; 0]">The providerOrganization, if present, SHALL contain at least one [1..*] id (CONF:1198-5417).</sch:assert>
      <sch:assert id="a-1198-5420" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization) or cda:recordTarget/cda:patientRole/cda:providerOrganization[count(cda:telecom) &gt; 0]">The providerOrganization, if present, SHALL contain at least one [1..*] telecom (CONF:1198-5420).</sch:assert>
      <sch:assert id="a-1198-5444" test="count(cda:author) &gt; 0">SHALL contain at least one [1..*] author (CONF:1198-5444).</sch:assert>
      <sch:assert id="a-1198-5448" test="cda:author[count(cda:assignedAuthor)=1]">Such authors SHALL contain exactly one [1..1] assignedAuthor (CONF:1198-5448).</sch:assert>
      <sch:assert id="a-1198-5428" test="cda:author/cda:assignedAuthor[count(cda:telecom) &gt; 0]">This assignedAuthor SHALL contain at least one [1..*] telecom (CONF:1198-5428).</sch:assert>
      <sch:assert id="a-1198-5442" test="not(cda:dataEnterer) or cda:dataEnterer[count(cda:assignedEntity)=1]">The dataEnterer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1198-5442).</sch:assert>
      <sch:assert id="a-1198-5443" test="not(cda:dataEnterer/cda:assignedEntity) or cda:dataEnterer/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:1198-5443).</sch:assert>
      <sch:assert id="a-1198-5466" test="not(cda:dataEnterer/cda:assignedEntity) or cda:dataEnterer/cda:assignedEntity[count(cda:telecom) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] telecom (CONF:1198-5466).</sch:assert>
      <sch:assert id="a-1198-5469" test="not(cda:dataEnterer/cda:assignedEntity) or cda:dataEnterer/cda:assignedEntity[count(cda:assignedPerson)=1]">This assignedEntity SHALL contain exactly one [1..1] assignedPerson (CONF:1198-5469).</sch:assert>
      <sch:assert id="a-1198-5519" test="count(cda:custodian)=1">SHALL contain exactly one [1..1] custodian (CONF:1198-5519).</sch:assert>
      <sch:assert id="a-1198-5520" test="cda:custodian[count(cda:assignedCustodian)=1]">This custodian SHALL contain exactly one [1..1] assignedCustodian (CONF:1198-5520).</sch:assert>
      <sch:assert id="a-1198-5521" test="cda:custodian/cda:assignedCustodian[count(cda:representedCustodianOrganization)=1]">This assignedCustodian SHALL contain exactly one [1..1] representedCustodianOrganization (CONF:1198-5521).</sch:assert>
      <sch:assert id="a-1198-5522" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization[count(cda:id) &gt; 0]">This representedCustodianOrganization SHALL contain at least one [1..*] id (CONF:1198-5522).</sch:assert>
      <sch:assert id="a-1198-5525" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization[count(cda:telecom)=1]">This representedCustodianOrganization SHALL contain exactly one [1..1] telecom (CONF:1198-5525).</sch:assert>
      <sch:assert id="a-1198-5566" test="not(cda:informationRecipient) or cda:informationRecipient[count(cda:intendedRecipient)=1]">The informationRecipient, if present, SHALL contain exactly one [1..1] intendedRecipient (CONF:1198-5566).</sch:assert>
      <sch:assert id="a-1198-5583" test="not(cda:legalAuthenticator) or cda:legalAuthenticator[count(cda:signatureCode)=1]">The legalAuthenticator, if present, SHALL contain exactly one [1..1] signatureCode (CONF:1198-5583).</sch:assert>
      <sch:assert id="a-1198-5585" test="not(cda:legalAuthenticator) or cda:legalAuthenticator[count(cda:assignedEntity)=1]">The legalAuthenticator, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1198-5585).</sch:assert>
      <sch:assert id="a-1198-5586" test="not(cda:legalAuthenticator/cda:assignedEntity) or cda:legalAuthenticator/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:1198-5586).</sch:assert>
      <sch:assert id="a-1198-5595" test="not(cda:legalAuthenticator/cda:assignedEntity) or cda:legalAuthenticator/cda:assignedEntity[count(cda:telecom) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] telecom (CONF:1198-5595).</sch:assert>
      <sch:assert id="a-1198-5597" test="not(cda:legalAuthenticator/cda:assignedEntity) or cda:legalAuthenticator/cda:assignedEntity[count(cda:assignedPerson)=1]">This assignedEntity SHALL contain exactly one [1..1] assignedPerson (CONF:1198-5597).</sch:assert>
      <sch:assert id="a-1198-9953" test="not(cda:inFulfillmentOf) or cda:inFulfillmentOf[count(cda:order)=1]">The inFulfillmentOf, if present, SHALL contain exactly one [1..1] order (CONF:1198-9953).</sch:assert>
      <sch:assert id="a-1198-14836" test="not(cda:documentationOf) or cda:documentationOf[count(cda:serviceEvent)=1]">The documentationOf, if present, SHALL contain exactly one [1..1] serviceEvent (CONF:1198-14836).</sch:assert>
      <sch:assert id="a-1198-14837" test="not(cda:documentationOf/cda:serviceEvent) or cda:documentationOf/cda:serviceEvent[count(cda:effectiveTime)=1]">This serviceEvent SHALL contain exactly one [1..1] effectiveTime (CONF:1198-14837).</sch:assert>
      <sch:assert id="a-1198-14841" test="not(cda:documentationOf/cda:serviceEvent/cda:performer) or cda:documentationOf/cda:serviceEvent/cda:performer[count(cda:assignedEntity)=1]">The performer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1198-14841).</sch:assert>
      <sch:assert id="a-1198-14846" test="not(cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity) or cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:1198-14846).</sch:assert>
      <sch:assert id="a-1198-9956" test="not(cda:componentOf) or cda:componentOf[count(cda:encompassingEncounter)=1]">The componentOf, if present, SHALL contain exactly one [1..1] encompassingEncounter (CONF:1198-9956).</sch:assert>
      <sch:assert id="a-1198-5256-c" test="not(existence_schema_tested)">SHALL contain exactly one [1..1] US Realm Date and Time (DTM.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.4) (CONF:1198-5256).</sch:assert>
      <sch:assert id="a-1198-16791" test="count(cda:realmCode[@code='US'])=1">SHALL contain exactly one [1..1] realmCode="US" (CONF:1198-16791).</sch:assert>
      <sch:assert id="a-1198-5250" test="cda:typeId[@root='2.16.840.1.113883.1.3']">This typeId SHALL contain exactly one [1..1] @root="2.16.840.1.113883.1.3" (CONF:1198-5250).</sch:assert>
      <sch:assert id="a-1198-5251" test="cda:typeId[@extension='POCD_HD000040']">This typeId SHALL contain exactly one [1..1] @extension="POCD_HD000040" (CONF:1198-5251).</sch:assert>
      <sch:assert id="a-1198-9992-c" test=".">This code **SHALL** specify the particular kind of document (e.g., History and Physical, Discharge Summary, Progress Note) (CONF:1198-9992).</sch:assert>
      <sch:assert id="a-1198-5254" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-5254).</sch:assert>
      <sch:assert id="a-1198-5259" test="count(cda:confidentialityCode)=1">SHALL contain exactly one [1..1] confidentialityCode, which SHOULD be selected from ValueSet HL7 BasicConfidentialityKind urn:oid:2.16.840.1.113883.1.11.16926 DYNAMIC (CONF:1198-5259).</sch:assert>
      <sch:assert id="a-1198-5372" test="count(cda:languageCode)=1">SHALL contain exactly one [1..1] languageCode, which SHALL be selected from ValueSet Language urn:oid:2.16.840.1.113883.1.11.11526 DYNAMIC (CONF:1198-5372).</sch:assert>
      <sch:assert id="a-1198-6380-c" test="count(cda:versionNumber |cda:setId)=2 or count(cda:versionNumber | cda:setId)=0">If  setId is present versionNumber **SHALL** be present (CONF:1198-6380).</sch:assert>
      <sch:assert id="a-1198-6387-c" test="count(cda:versionNumber |cda:setId)=2 or count(cda:versionNumber | cda:setId)=0">If versionNumber is present setId **SHALL** be present (CONF:1198-6387).</sch:assert>
      <sch:assert id="a-1198-5268" test="cda:recordTarget/cda:patientRole[count(cda:id) &gt; 0]">This patientRole SHALL contain at least one [1..*] id (CONF:1198-5268).</sch:assert>
      <sch:assert id="a-1198-5271-c" test="cda:recordTarget/cda:patientRole[count(cda:addr) &gt; 0]">This patientRole SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5271).</sch:assert>
      <sch:assert id="a-1198-5284-c" test="count(cda:recordTarget/cda:patientRole/cda:patient) &lt;= count(cda:recordTarget/cda:patientRole/cda:patient/cda:name)">This patient SHALL contain at least one [1..*] US Realm Patient Name (PTN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1) (CONF:1198-5284).</sch:assert>
      <sch:assert id="a-1198-6394" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:administrativeGenderCode)=1]">This patient SHALL contain exactly one [1..1] administrativeGenderCode, which SHALL be selected from ValueSet Administrative Gender (HL7 V3) urn:oid:2.16.840.1.113883.1.11.1 DYNAMIC (CONF:1198-6394).</sch:assert>
      <sch:assert id="a-1198-5299-c" test="cda:recordTarget/cda:patientRole/cda:patient/cda:birthTime/@nullFlavor or string-length(cda:recordTarget/cda:patientRole/cda:patient/cda:birthTime/@value) &gt;= 4">**SHALL** be precise to year (CONF:1198-5299).</sch:assert>
      <sch:assert id="a-1198-5322" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:raceCode)=1]">This patient SHALL contain exactly one [1..1] raceCode, which SHALL be selected from ValueSet Race Category Excluding Nulls urn:oid:2.16.840.1.113883.3.2074.1.1.3 DYNAMIC (CONF:1198-5322).</sch:assert>
      <sch:assert id="a-1198-5323" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:ethnicGroupCode)=1]">This patient SHALL contain exactly one [1..1] ethnicGroupCode, which SHALL be selected from ValueSet Ethnicity urn:oid:2.16.840.1.114222.4.11.837 DYNAMIC (CONF:1198-5323).</sch:assert>
      <sch:assert id="a-1198-5386-c" test="not(tested_here)">This guardianPerson SHALL contain at least one [1..*] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1198-5386).</sch:assert>
      <sch:assert id="a-1198-5407" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication) or cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication[count(cda:languageCode)=1]">The languageCommunication, if present, SHALL contain exactly one [1..1] languageCode, which SHALL be selected from ValueSet Language urn:oid:2.16.840.1.113883.1.11.11526 DYNAMIC (CONF:1198-5407).</sch:assert>
      <sch:assert id="a-1198-31347-c" test="not(cda:recordTarget/cda:patientRole/cda:patient/sdtc:raceCode) or cda:recordTarget/cda:patientRole/cda:patient/cda:raceCode">If sdtc:raceCode is present, then the patient **SHALL** contain [1..1] raceCode (CONF:1198-31347).</sch:assert>
      <sch:assert id="a-1198-5419" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization) or cda:recordTarget/cda:patientRole/cda:providerOrganization[count(cda:name) &gt; 0]">The providerOrganization, if present, SHALL contain at least one [1..*] name (CONF:1198-5419).</sch:assert>
      <sch:assert id="a-1198-5422-c" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization) or cda:recordTarget/cda:patientRole/cda:providerOrganization[count(cda:addr) &gt; 0]">The providerOrganization, if present, SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5422).</sch:assert>
      <sch:assert id="a-1198-5445-c" test="not(existence_schema_tested)">Such authors SHALL contain exactly one [1..1] US Realm Date and Time (DTM.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.4) (CONF:1198-5445).</sch:assert>
      <sch:assert id="a-1198-5449" test="cda:author/cda:assignedAuthor[count(cda:id) &gt; 0]">This assignedAuthor SHALL contain at least one [1..*] id (CONF:1198-5449).</sch:assert>
      <sch:assert id="a-1198-16788" test="not(cda:author/cda:assignedAuthor/cda:code) or cda:author/cda:assignedAuthor/cda:code[@code]">The code, if present, SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Healthcare Provider Taxonomy urn:oid:2.16.840.1.114222.4.11.1066 DYNAMIC (CONF:1198-16788).</sch:assert>
      <sch:assert id="a-1198-5452-c" test="count(cda:author/cda:assignedAuthor) &lt;= count(cda:author/cda:assignedAuthor/cda:addr)">This assignedAuthor SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5452).</sch:assert>
      <sch:assert id="a-1198-16789-c" test="not(tested_here)">The assignedPerson, if present, SHALL contain at least one [1..*] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1198-16789).</sch:assert>
      <sch:assert id="a-1198-16784" test="not(cda:author/cda:assignedAuthor/cda:assignedAuthoringDevice) or cda:author/cda:assignedAuthor/cda:assignedAuthoringDevice[count(cda:manufacturerModelName)=1]">The assignedAuthoringDevice, if present, SHALL contain exactly one [1..1] manufacturerModelName (CONF:1198-16784).</sch:assert>
      <sch:assert id="a-1198-16785" test="not(cda:author/cda:assignedAuthor/cda:assignedAuthoringDevice) or cda:author/cda:assignedAuthor/cda:assignedAuthoringDevice[count(cda:softwareName)=1]">The assignedAuthoringDevice, if present, SHALL contain exactly one [1..1] softwareName (CONF:1198-16785).</sch:assert>
      <sch:assert id="a-1198-16790-c" test="cda:author/cda:assignedAuthor[count(cda:assignedPerson |cda:assignedAuthoringDevice)=1] and not(cda:author/cda:assignedAuthor[count(cda:assignedPerson |cda:assignedAuthoringDevice)!=1] )">There **SHALL** be exactly one assignedAuthor/assignedPerson or exactly one assignedAuthor/assignedAuthoringDevice (CONF:1198-16790).</sch:assert>
      <sch:assert id="a-1198-5460-c" test="count(cda:dataEnterer/cda:assignedEntity) &lt;= count(cda:dataEnterer/cda:assignedEntity/cda:addr)">This assignedEntity SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5460).</sch:assert>
      <sch:assert id="a-1198-5470-c" test="count(cda:dataEnterer/cda:assignedEntity/cda:assignedPerson) &lt;= count(cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name)">This assignedPerson SHALL contain at least one [1..*] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1198-5470).</sch:assert>
      <sch:assert id="a-1198-5524" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization[count(cda:name)=1]">This representedCustodianOrganization SHALL contain exactly one [1..1] name (CONF:1198-5524).</sch:assert>
      <sch:assert id="a-1198-5559-c" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization[count(cda:addr)=1]">This representedCustodianOrganization SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5559).</sch:assert>
      <sch:assert id="a-1198-5568-c" test="count(cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient) &lt;= count(cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name)">The informationRecipient, if present, SHALL contain at least one [1..*] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1198-5568).</sch:assert>
      <sch:assert id="a-1198-5578" test="not(cda:informationRecipient/cda:intendedRecipient/cda:receivedOrganization) or cda:informationRecipient/cda:intendedRecipient/cda:receivedOrganization[count(cda:name)=1]">The receivedOrganization, if present, SHALL contain exactly one [1..1] name (CONF:1198-5578).</sch:assert>
      <sch:assert id="a-1198-5580-c" test="not(existence_schema_tested)">The legalAuthenticator, if present, SHALL contain exactly one [1..1] US Realm Date and Time (DTM.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.4) (CONF:1198-5580).</sch:assert>
      <sch:assert id="a-1198-5584" test="not(cda:legalAuthenticator/cda:signatureCode) or cda:legalAuthenticator/cda:signatureCode[@code='S']">This signatureCode SHALL contain exactly one [1..1] @code="S" (CodeSystem: HL7ParticipationSignature urn:oid:2.16.840.1.113883.5.89 STATIC) (CONF:1198-5584).</sch:assert>
      <sch:assert id="a-1198-5589-c" test="not(cda:legalAuthenticator) or cda:legalAuthenticator/cda:assignedEntity[count(cda:addr) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5589).</sch:assert>
      <sch:assert id="a-1198-5598-c" test="not(tested_here)">This assignedPerson SHALL contain at least one [1..*] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:1198-5598).</sch:assert>
      <sch:assert id="a-1198-10006-c" test="count(cda:participant) = count( cda:participant/cda:associatedEntity[cda:associatedPerson | cda:scopingOrganization])">**SHALL** contain associatedEntity/associatedPerson *AND/OR* associatedEntity/scopingOrganization (CONF:1198-10006).</sch:assert>
      <sch:assert id="a-1198-9954" test="not(cda:inFulfillmentOf/cda:order) or cda:inFulfillmentOf/cda:order[count(cda:id) &gt; 0]">This order SHALL contain at least one [1..*] id (CONF:1198-9954).</sch:assert>
      <sch:assert id="a-1198-14838" test="not(cda:documentationOf/cda:serviceEvent/cda:effectiveTime) or cda:documentationOf/cda:serviceEvent/cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:1198-14838).</sch:assert>
      <sch:assert id="a-1198-14840" test="not(cda:documentationOf/cda:serviceEvent/cda:performer) or cda:documentationOf/cda:serviceEvent/cda:performer[@typeCode and @typeCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.1.11.19601']/voc:code/@value]">The performer, if present, SHALL contain exactly one [1..1] @typeCode, which SHALL be selected from ValueSet x_ServiceEventPerformer urn:oid:2.16.840.1.113883.1.11.19601 STATIC (CONF:1198-14840).</sch:assert>
      <sch:assert id="a-1198-9959" test="not(cda:componentOf/cda:encompassingEncounter) or cda:componentOf/cda:encompassingEncounter[count(cda:id) &gt; 0]">This encompassingEncounter SHALL contain at least one [1..*] id (CONF:1198-9959).</sch:assert>
      <sch:assert id="a-1198-9958" test="not(cda:componentOf/cda:encompassingEncounter) or cda:componentOf/cda:encompassingEncounter[count(cda:effectiveTime)=1]">This encompassingEncounter SHALL contain exactly one [1..1] effectiveTime (CONF:1198-9958).</sch:assert>
      <sch:assert id="a-1198-32948-c" test=".">This code **SHALL** be drawn from the LOINC document type ontology (LOINC codes where SCALE = DOC) (CONF:1198-32948).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-5252" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-5252) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.1.1" (CONF:1198-10036). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32503).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-8712" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.49'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-8712) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.49" (CONF:1198-26353). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32546).</sch:assert>
      <sch:assert id="a-1198-8714" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet EncounterTypeCode urn:oid:2.16.840.1.113883.3.88.12.80.32 DYNAMIC (CONF:1198-8714).</sch:assert>
      <sch:assert id="a-1198-8726" test="not(cda:performer) or cda:performer[count(cda:assignedEntity)=1]">The performer, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:1198-8726).</sch:assert>
      <sch:assert id="a-1198-8710" test="@classCode='ENC'">SHALL contain exactly one [1..1] @classCode="ENC" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-8710).</sch:assert>
      <sch:assert id="a-1198-8711" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-8711).</sch:assert>
      <sch:assert id="a-1198-8713" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:1198-8713).</sch:assert>
      <sch:assert id="a-1198-15972-c" test="count(cda:code/cda:originalText/cda:reference[@value])=0 or starts-with(cda:code/cda:originalText/cda:reference/@value, '#')">This reference/@value **SHALL** begin with a '#' and **SHALL** point to its corresponding narrative (using the approach defined in CDA Release 2, section 4.3.5.1) (CONF:1198-15972).</sch:assert>
      <sch:assert id="a-1198-8715" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:1198-8715).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-errors" context="cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.49' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors-abstract" abstract="true">
      <sch:assert id="a-1198-15461" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15461).</sch:assert>
      <sch:assert id="a-1198-15462" test="cda:code[@code='46240-8']">This code SHALL contain exactly one [1..1] @code="46240-8" Encounters (CONF:1198-15462).</sch:assert>
      <sch:assert id="a-1198-31136" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:1198-31136).</sch:assert>
      <sch:assert id="a-1198-7942" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-7942).</sch:assert>
      <sch:assert id="a-1198-7943" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-7943).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-7940" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7940) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.22" (CONF:1198-10386). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32547).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-8709-c" test="(cda:entry/cda:encounter/cda:templateId[@root='2.16.840.1.113883.10.20.22.4.49'][@extension='2015-08-01'] or @nullFlavor) and not( cda:entry and  @nullFlavor)">SHALL contain at least one [1..*] entry (CONF:1198-8709) such that it SHALL contain exactly one [1..1] Encounter Activity (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.49:2015-08-01) (CONF:1198-15468).</sch:assert>
      <sch:assert id="a-1198-15466" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-15466).</sch:assert>
      <sch:assert id="a-1198-15467" test="cda:code[@code='46240-8']">This code SHALL contain exactly one [1..1] @code="46240-8" Encounters (CONF:1198-15467).</sch:assert>
      <sch:assert id="a-1198-31137" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem=" 2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:1198-31137).</sch:assert>
      <sch:assert id="a-1198-8707" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:1198-8707).</sch:assert>
      <sch:assert id="a-1198-8708" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:1198-8708).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-1198-8705" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22.1'][@extension='2015-08-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-8705) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.22.1" (CONF:1198-10387). SHALL contain exactly one [1..1] @extension="2015-08-01" (CONF:1198-32548).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-errors-abstract" abstract="true">
      <sch:assert id="a-3250-18124" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3250-18124).</sch:assert>
      <sch:assert id="a-3250-18232" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.200'][@extension='2016-06-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3250-18232) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.200" (CONF:3250-18233). SHALL contain exactly one [1..1] @extension="2016-06-01" (CONF:3250-32949).</sch:assert>
      <sch:assert id="a-3250-18234" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3250-18234).</sch:assert>
      <sch:assert id="a-3250-32947" test="count(cda:value[@xsi:type='CD' and @code=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113762.1.4.1']/voc:code/@value or @nullFlavor])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet ONC Administrative Sex urn:oid:2.16.840.1.113762.1.4.1 STATIC 2016-06-01 (CONF:3250-32947).</sch:assert>
      <sch:assert id="a-3250-18125" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:3250-18125).</sch:assert>
      <sch:assert id="a-3250-18235" test="cda:code[@code='76689-9']">This code SHALL contain exactly one [1..1] @code="76689-9" Sex Assigned At Birth (CONF:3250-18235).</sch:assert>
      <sch:assert id="a-3250-21163" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3250-21163).</sch:assert>
      <sch:assert id="a-3250-18230" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3250-18230).</sch:assert>
      <sch:assert id="a-3250-18231" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3250-18231).</sch:assert>
      <sch:assert id="a-3250-32948-c" test="not(tested)">If value/@code not from value set ONC Administrative Sex urn:oid:2.16.840.1.113762.1.4.1 STATIC 2016-06-01, then value/@nullFlavor SHALL be “UNK” (CONF:3250-32948).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.200' and @extension='2016-06-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-131" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.34'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-131) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.34" (CONF:3315-137). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-138).</sch:assert>
      <sch:assert id="a-3315-132" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-132).</sch:assert>
      <sch:assert id="a-3315-139" test="cda:code[@code='RR11']">This code SHALL contain exactly one [1..1] @code="RR11" Reportability Response Coded Information (CONF:3315-139).</sch:assert>
      <sch:assert id="a-3315-140" test="cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-140).</sch:assert>
      <sch:assert id="a-3315-703" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.12' and @extension='2017-04-01']])=1]) &gt; 0">SHALL contain at least one [1..*] component (CONF:3315-703) such that it SHALL contain exactly one [1..1] Relevant Reportable Condition Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.12:2017-04-01) (CONF:3315-704).</sch:assert>
      <sch:assert id="a-3315-707" test="@classCode='CLUSTER'">SHALL contain exactly one [1..1] @classCode="CLUSTER" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3315-707).</sch:assert>
      <sch:assert id="a-3315-708" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3315-708).</sch:assert>
      <sch:assert id="a-3315-709" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3315-709).</sch:assert>
      <sch:assert id="a-3315-710" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3315-710).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.34' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-217" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.12'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-217) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.12" (CONF:3315-225). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-226).</sch:assert>
      <sch:assert id="a-3315-233" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3315-233).</sch:assert>
      <sch:assert id="a-3315-234" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3315-234).</sch:assert>
      <sch:assert id="a-3315-434" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-434).</sch:assert>
      <sch:assert id="a-3315-548" test="count(cda:entryRelationship[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.13' and @extension='2017-04-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entryRelationship (CONF:3315-548) such that it SHALL contain exactly one [1..1] Reportability Information Organizer (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.13:2017-04-01) (CONF:3315-550).</sch:assert>
      <sch:assert id="a-3315-552" test="count(cda:value[@codeSystem='2.16.840.1.113883.6.96' or @nullFlavor][@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from CodeSystem SNOMED CT (urn:oid:2.16.840.1.113883.6.96) DYNAMIC (CONF:3315-552).</sch:assert>
      <sch:assert id="a-3315-553" test="cda:code[count(cda:translation)=1]">This code SHALL contain exactly one [1..1] translation (CONF:3315-553).</sch:assert>
      <sch:assert id="a-3315-554" test="cda:code[@code='64572001']">This code SHALL contain exactly one [1..1] @code="64572001" Condition (CONF:3315-554).</sch:assert>
      <sch:assert id="a-3315-555" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:3315-555).</sch:assert>
      <sch:assert id="a-3315-556" test="cda:code/cda:translation[@code='75323-6']">This translation SHALL contain exactly one [1..1] @code="75323-6" Condition (CONF:3315-556).</sch:assert>
      <sch:assert id="a-3315-557" test="cda:code/cda:translation[@codeSystem='2.16.840.1.113883.6.1']">This translation SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3315-557).</sch:assert>
      <sch:assert id="a-3315-736-c" test="cda:value[@nullFlavor] or cda:value[@code][@codeSystem][@displayName]">If @code is present then @codeSystem and @displayName must both be present (CONF:3315-736).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.12' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-237" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.13'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-237) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.13" (CONF:3315-239). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-240).</sch:assert>
      <sch:assert id="a-3315-238" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19' and @extension='2017-04-01']])=1])=1">SHALL contain exactly one [1..1] component (CONF:3315-238) such that it SHALL contain exactly one [1..1] Determination of Reportability (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.19:2017-04-01) (CONF:3315-329).</sch:assert>
      <sch:assert id="a-3315-241" test="@classCode='CLUSTER'">SHALL contain exactly one [1..1] @classCode="CLUSTER" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3315-241).</sch:assert>
      <sch:assert id="a-3315-242" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3315-242).</sch:assert>
      <sch:assert id="a-3315-245" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3315-245).</sch:assert>
      <sch:assert id="a-3315-246" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3315-246).</sch:assert>
      <sch:assert id="a-3315-429" test="count(cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.3' and @extension='2017-04-01']]) &gt; 0">SHALL contain at least one [1..*] Rules Authoring Agency (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.3:2017-04-01) (CONF:3315-429).</sch:assert>
      <sch:assert id="a-3315-543" test="count(cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.1' and @extension='2017-04-01']]) &gt; 0">SHALL contain at least one [1..*] Routing Entity (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.1:2017-04-01) (CONF:3315-543).</sch:assert>
      <sch:assert id="a-3315-581" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Location Relevance (eCR) urn:oid:2.16.840.1.113883.10.20.15.2.5.6 DYNAMIC (CONF:3315-581).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.13' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-249" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.14'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-249) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.14" (CONF:3315-251). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-252).</sch:assert>
      <sch:assert id="a-3315-250" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-250).</sch:assert>
      <sch:assert id="a-3315-253" test="cda:code[@code='RR4']">This code SHALL contain exactly one [1..1] @code="RR4" Timeframe to report (urgency) (CONF:3315-253).</sch:assert>
      <sch:assert id="a-3315-254" test="cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-254).</sch:assert>
      <sch:assert id="a-3315-255" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:3315-255).</sch:assert>
      <sch:assert id="a-3315-256" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3315-256).</sch:assert>
      <sch:assert id="a-3315-257" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3315-257).</sch:assert>
      <sch:assert id="a-3315-671" test="cda:value[@xsi:type='PQ'][@value]">This value SHALL contain exactly one [1..1] @value (CONF:3315-671).</sch:assert>
      <sch:assert id="a-3315-672" test="cda:value[@xsi:type='PQ'][@unit]">This value SHALL contain exactly one [1..1] @unit, which SHOULD be selected from CodeSystem UCUM (urn:oid:2.16.840.1.113883.6.8) DYNAMIC (CONF:3315-672).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.14' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-271" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.17'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-271) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.17" (CONF:3315-274). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-275).</sch:assert>
      <sch:assert id="a-3315-272" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-272).</sch:assert>
      <sch:assert id="a-3315-276" test="@classCode='DOC'">SHALL contain exactly one [1..1] @classCode="DOC" Document (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3315-276).</sch:assert>
      <sch:assert id="a-3315-277" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3315-277).</sch:assert>
      <sch:assert id="a-3315-280" test="not(cda:text) or cda:text[count(cda:reference)=1]">The text, if present, SHALL contain exactly one [1..1] reference (CONF:3315-280).</sch:assert>
      <sch:assert id="a-3315-281" test="cda:code[count(cda:originalText)=1]">This code SHALL contain exactly one [1..1] originalText (CONF:3315-281).</sch:assert>
      <sch:assert id="a-3315-285" test="cda:code[@nullFlavor='OTH']">This code SHALL contain exactly one [1..1] @nullFlavor="OTH" (CodeSystem: HL7NullFlavor urn:oid:2.16.840.1.113883.5.1008) (CONF:3315-285).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-errors" context="cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.17' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-347" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-347) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.19" (CONF:3315-349). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-350).</sch:assert>
      <sch:assert id="a-3315-348" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-348).</sch:assert>
      <sch:assert id="a-3315-351" test="cda:code[@code='RR1']">This code SHALL contain exactly one [1..1] @code="RR1" Determination of reportability (CONF:3315-351).</sch:assert>
      <sch:assert id="a-3315-352" test="cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-352).</sch:assert>
      <sch:assert id="a-3315-353" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Determination of Reportability (eCR) urn:oid:2.16.840.1.113883.10.20.15.2.5.3 DYNAMIC (CONF:3315-353).</sch:assert>
      <sch:assert id="a-3315-354" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3315-354).</sch:assert>
      <sch:assert id="a-3315-355" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3315-355).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-533-branch-533-errors-abstract" abstract="true">
      <sch:assert id="a-3315-535-branch-533" test="@typeCode='RSON'">SHALL contain exactly one [1..1] @typeCode="RSON" Reason (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-535).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-533-branch-533-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19' and @extension='2017-04-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-533-branch-533-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-534-branch-534-errors-abstract" abstract="true">
      <sch:assert id="a-3315-537-branch-534" test="@typeCode='RSON'">SHALL contain exactly one [1..1] @typeCode="RSON" Reason (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-537).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-534-branch-534-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19' and @extension='2017-04-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-534-branch-534-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-387" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.1'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-387) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.1" (CONF:3315-389). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-390).</sch:assert>
      <sch:assert id="a-3315-388" test="@typeCode='LOC'">SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-388).</sch:assert>
      <sch:assert id="a-3315-391" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:3315-391).</sch:assert>
      <sch:assert id="a-3315-393" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:3315-393).</sch:assert>
      <sch:assert id="a-3315-399" test="cda:participantRole/cda:playingEntity[count(cda:name)=1]">This playingEntity SHALL contain exactly one [1..1] name (CONF:3315-399).</sch:assert>
      <sch:assert id="a-3315-392" test="cda:participantRole[count(cda:code)=1]">This participantRole SHALL contain exactly one [1..1] code (CONF:3315-392).</sch:assert>
      <sch:assert id="a-3315-395" test="cda:participantRole/cda:code[@code='RR7']">This code SHALL contain exactly one [1..1] @code="RR7" Routing Enitity (CONF:3315-395).</sch:assert>
      <sch:assert id="a-3315-396" test="cda:participantRole/cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-396).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.1' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-401" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.2'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-401) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.2" (CONF:3315-405). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-406).</sch:assert>
      <sch:assert id="a-3315-402" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:3315-402).</sch:assert>
      <sch:assert id="a-3315-404" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:3315-404).</sch:assert>
      <sch:assert id="a-3315-407" test="@typeCode='LOC'">SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-407).</sch:assert>
      <sch:assert id="a-3315-410" test="cda:participantRole/cda:playingEntity[count(cda:name)=1]">This playingEntity SHALL contain exactly one [1..1] name (CONF:3315-410).</sch:assert>
      <sch:assert id="a-3315-413" test="cda:participantRole[count(cda:addr) &gt; 0]">This participantRole SHALL contain at least one [1..*] addr (CONF:3315-413).</sch:assert>
      <sch:assert id="a-3315-414" test="cda:participantRole[count(cda:telecom) &gt; 0]">This participantRole SHALL contain at least one [1..*] telecom (CONF:3315-414).</sch:assert>
      <sch:assert id="a-3315-403" test="cda:participantRole[count(cda:code)=1]">This participantRole SHALL contain exactly one [1..1] code (CONF:3315-403).</sch:assert>
      <sch:assert id="a-3315-408" test="cda:participantRole/cda:code[@code='RR8']">This code SHALL contain exactly one [1..1] @code="RR8" Responsible Agency (CONF:3315-408).</sch:assert>
      <sch:assert id="a-3315-409" test="cda:participantRole/cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-409).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.2' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-415" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.3'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-415) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.3" (CONF:3315-419). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-420).</sch:assert>
      <sch:assert id="a-3315-416" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:3315-416).</sch:assert>
      <sch:assert id="a-3315-418" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:3315-418).</sch:assert>
      <sch:assert id="a-3315-423" test="cda:participantRole/cda:playingEntity[count(cda:name)=1]">This playingEntity SHALL contain exactly one [1..1] name (CONF:3315-423).</sch:assert>
      <sch:assert id="a-3315-426" test="cda:participantRole[count(cda:addr) &gt; 0]">This participantRole SHALL contain at least one [1..*] addr (CONF:3315-426).</sch:assert>
      <sch:assert id="a-3315-427" test="cda:participantRole[count(cda:telecom) &gt; 0]">This participantRole SHALL contain at least one [1..*] telecom (CONF:3315-427).</sch:assert>
      <sch:assert id="a-3315-428" test="@typeCode='LOC'">SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-428).</sch:assert>
      <sch:assert id="a-3315-417" test="cda:participantRole[count(cda:code)=1]">This participantRole SHALL contain exactly one [1..1] code (CONF:3315-417).</sch:assert>
      <sch:assert id="a-3315-421" test="cda:participantRole/cda:code[@code='RR12']">This code SHALL contain exactly one [1..1] @code="RR12" Rules Authoring Agency (CONF:3315-421).</sch:assert>
      <sch:assert id="a-3315-422" test="cda:participantRole/cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-422).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.3' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-435" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.20'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-435) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.20" (CONF:3315-440). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-450).</sch:assert>
      <sch:assert id="a-3315-436" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet External Resource Type (eCR) urn:oid:2.16.840.1.113883.10.20.15.2.5.4 DYNAMIC (CONF:3315-436).</sch:assert>
      <sch:assert id="a-3315-438" test="count(cda:reference) &gt; 0">SHALL contain at least one [1..*] reference (CONF:3315-438).</sch:assert>
      <sch:assert id="a-3315-444" test="cda:reference[@typeCode='REFR']">Such references SHALL contain exactly one [1..1] @typeCode="REFR" refers to (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3315-444).</sch:assert>
      <sch:assert id="a-3315-448" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3315-448).</sch:assert>
      <sch:assert id="a-3315-449" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3315-449).</sch:assert>
      <sch:assert id="a-3315-451" test="cda:reference[count(cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.17' and @extension='2017-04-01']])=1]">Such references SHALL contain exactly one [1..1] External Reference (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.17:2017-04-01) (CONF:3315-451).</sch:assert>
      <sch:assert id="a-3315-454" test="count(cda:priorityCode)=1">SHALL contain exactly one [1..1] priorityCode (CONF:3315-454).</sch:assert>
      <sch:assert id="a-3315-597" test="count(cda:id)=1">SHALL contain exactly one [1..1] id (CONF:3315-597).</sch:assert>
      <sch:assert id="a-3315-626" test="cda:priorityCode[not(@nullFlavor)]">This priorityCode SHALL NOT contain [0..0] @nullFlavor (CONF:3315-626).</sch:assert>
      <sch:assert id="a-3315-627" test="cda:priorityCode[@code]">This priorityCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Reportability Priority (eCR) urn:oid:2.16.840.1.113883.10.20.15.2.5.5 DYNAMIC (CONF:3315-627).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.20' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-514" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.26'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-514) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.26" (CONF:3315-517). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-518).</sch:assert>
      <sch:assert id="a-3315-515" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-515).</sch:assert>
      <sch:assert id="a-3315-519" test="cda:code[@code='RR2']">This code SHALL contain exactly one [1..1] @code="RR2" Determination of Reportability Reason (CONF:3315-519).</sch:assert>
      <sch:assert id="a-3315-520" test="cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-520).</sch:assert>
      <sch:assert id="a-3315-521" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:3315-521).</sch:assert>
      <sch:assert id="a-3315-522" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3315-522).</sch:assert>
      <sch:assert id="a-3315-523" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3315-523).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.26' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3315-524" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.27'][@extension='2017-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3315-524) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.27" (CONF:3315-526). SHALL contain exactly one [1..1] @extension="2017-04-01" (CONF:3315-527).</sch:assert>
      <sch:assert id="a-3315-525" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-525).</sch:assert>
      <sch:assert id="a-3315-528" test="cda:code[@code='RR3']">This code SHALL contain exactly one [1..1] @code="RR3" Determination of reportability rule (CONF:3315-528).</sch:assert>
      <sch:assert id="a-3315-529" test="cda:code[@codeSystem='2.16.840.1.114222.4.5.232']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.114222.4.5.232" (CodeSystem: PHIN Questions urn:oid:2.16.840.1.114222.4.5.232) (CONF:3315-529).</sch:assert>
      <sch:assert id="a-3315-530" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:3315-530).</sch:assert>
      <sch:assert id="a-3315-531" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3315-531).</sch:assert>
      <sch:assert id="a-3315-532" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3315-532).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.27' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-10" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.212'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-10) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.212" (CONF:3349-13). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-14).</sch:assert>
      <sch:assert id="a-3349-11" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-11).</sch:assert>
      <sch:assert id="a-3349-12" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3349-12).</sch:assert>
      <sch:assert id="a-3349-15" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-15).</sch:assert>
      <sch:assert id="a-3349-16" test="cda:code[@code='74165-2' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="74165-2" History of Employment Status (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-16).</sch:assert>
      <sch:assert id="a-3349-17" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-17).</sch:assert>
      <sch:assert id="a-3349-18" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-18).</sch:assert>
      <sch:assert id="a-3349-19" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:3349-19).</sch:assert>
      <sch:assert id="a-3349-21" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Employment Status ODH urn:oid:2.16.840.1.113883.1.11.20562 DYNAMIC (CONF:3349-21).</sch:assert>
      <sch:assert id="a-3349-192" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-192).</sch:assert>
      <sch:assert id="a-3349-193" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-193).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.212' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-46" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.220'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-46) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.220" (CONF:3349-49). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-50).</sch:assert>
      <sch:assert id="a-3349-47" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-47).</sch:assert>
      <sch:assert id="a-3349-48" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:3349-48).</sch:assert>
      <sch:assert id="a-3349-51" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-51).</sch:assert>
      <sch:assert id="a-3349-52" test="cda:code[@code='74163-7' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="74163-7" Usual Occupation Duration (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-52).</sch:assert>
      <sch:assert id="a-3349-53" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-53).</sch:assert>
      <sch:assert id="a-3349-54" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-54).</sch:assert>
      <sch:assert id="a-3349-56" test="cda:value[@xsi:type='PQ'][@unit='a']">This value SHALL contain exactly one [1..1] @unit="a" years (CodeSystem: UCUM urn:oid:2.16.840.1.113883.6.8) (CONF:3349-56).</sch:assert>
      <sch:assert id="a-3349-204" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-204).</sch:assert>
      <sch:assert id="a-3349-205" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-205).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.220' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-57" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.218'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-57) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.218" (CONF:3349-61). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-62).</sch:assert>
      <sch:assert id="a-3349-59" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-59).</sch:assert>
      <sch:assert id="a-3349-60" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-60).</sch:assert>
      <sch:assert id="a-3349-63" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-63).</sch:assert>
      <sch:assert id="a-3349-64" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-64).</sch:assert>
      <sch:assert id="a-3349-65" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-65).</sch:assert>
      <sch:assert id="a-3349-67" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:3349-67).</sch:assert>
      <sch:assert id="a-3349-196" test="cda:code[@code='87510-4' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="87510-4" Date of retirement (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-196).</sch:assert>
      <sch:assert id="a-3349-197" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-197).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.218' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-68" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.213'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-68) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.213" (CONF:3349-70). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-71).</sch:assert>
      <sch:assert id="a-3349-69" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-69).</sch:assert>
      <sch:assert id="a-3349-72" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-72).</sch:assert>
      <sch:assert id="a-3349-73" test="cda:code[@code='87511-2' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="87511-2" Combat zone &amp;or hazardous duty work dates (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-73).</sch:assert>
      <sch:assert id="a-3349-74" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-74).</sch:assert>
      <sch:assert id="a-3349-198" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CONF:3349-198).</sch:assert>
      <sch:assert id="a-3349-199" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CONF:3349-199).</sch:assert>
      <sch:assert id="a-3349-200" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CONF:3349-200).</sch:assert>
      <sch:assert id="a-3349-293" test="count(cda:value[@xsi:type='IVL_TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="IVL_TS" (CONF:3349-293).</sch:assert>
      <sch:assert id="a-3349-294" test="cda:value[@xsi:type='IVL_TS'][count(cda:low)=1]">This value SHALL contain exactly one [1..1] low (CONF:3349-294).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.213' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-76" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.214'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-76) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.214" (CONF:3349-80). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-81).</sch:assert>
      <sch:assert id="a-3349-77" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-77).</sch:assert>
      <sch:assert id="a-3349-78" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-78).</sch:assert>
      <sch:assert id="a-3349-79" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-79).</sch:assert>
      <sch:assert id="a-3349-82" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-82).</sch:assert>
      <sch:assert id="a-3349-83" test="cda:code[@code='63761-1' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="63761-1" What were your main activities or duties for this job (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-83).</sch:assert>
      <sch:assert id="a-3349-84" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-84).</sch:assert>
      <sch:assert id="a-3349-85" test="count(cda:value[@xsi:type='ST'])=1">SHALL contain exactly one [1..1] value with @xsi:type="ST" (CONF:3349-85).</sch:assert>
      <sch:assert id="a-3349-211" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-211).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.214' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-89" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.215'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-89) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.215" (CONF:3349-93). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-94).</sch:assert>
      <sch:assert id="a-3349-90" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-90).</sch:assert>
      <sch:assert id="a-3349-91" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-91).</sch:assert>
      <sch:assert id="a-3349-92" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-92).</sch:assert>
      <sch:assert id="a-3349-95" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-95).</sch:assert>
      <sch:assert id="a-3349-96" test="cda:code[@code='87729-0' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="87729-0" History of Occupational hazard (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-96).</sch:assert>
      <sch:assert id="a-3349-97" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-97).</sch:assert>
      <sch:assert id="a-3349-98" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-98).</sch:assert>
      <sch:assert id="a-3349-99" test="count(cda:value[@xsi:type='ST'])=1">SHALL contain exactly one [1..1] value with @xsi:type="ST" (CONF:3349-99).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.215' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-104" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.222'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-104) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.222" (CONF:3349-108). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-109).</sch:assert>
      <sch:assert id="a-3349-105" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-105).</sch:assert>
      <sch:assert id="a-3349-106" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:3349-106).</sch:assert>
      <sch:assert id="a-3349-110" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-110).</sch:assert>
      <sch:assert id="a-3349-111" test="cda:code[@code='74160-3' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="74160-3" Work Days per Week (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-111).</sch:assert>
      <sch:assert id="a-3349-112" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-112).</sch:assert>
      <sch:assert id="a-3349-113" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-113).</sch:assert>
      <sch:assert id="a-3349-114" test="cda:value[@xsi:type='PQ'][@unit='d']">This value SHALL contain exactly one [1..1] @unit="d" day (CodeSystem: UCUM urn:oid:2.16.840.1.113883.6.8) (CONF:3349-114).</sch:assert>
      <sch:assert id="a-3349-214" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-214).</sch:assert>
      <sch:assert id="a-3349-215" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-215).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.222' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-115" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.211'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-115) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.211" (CONF:3349-118). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-119).</sch:assert>
      <sch:assert id="a-3349-116" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-116).</sch:assert>
      <sch:assert id="a-3349-117" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:3349-117).</sch:assert>
      <sch:assert id="a-3349-120" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-120).</sch:assert>
      <sch:assert id="a-3349-121" test="cda:code[@code='87512-0' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="87512-0" Work Hours per Day (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-121).</sch:assert>
      <sch:assert id="a-3349-122" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-122).</sch:assert>
      <sch:assert id="a-3349-123" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-123).</sch:assert>
      <sch:assert id="a-3349-212" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-212).</sch:assert>
      <sch:assert id="a-3349-213" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-213).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.211' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-126" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223'][@extension='2016-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-126) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.223" (CONF:3349-128). SHALL contain exactly one [1..1] @extension="2016-11-30" (CONF:3349-129).</sch:assert>
      <sch:assert id="a-3349-127" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-127).</sch:assert>
      <sch:assert id="a-3349-130" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-130).</sch:assert>
      <sch:assert id="a-3349-131" test="cda:code[@code='74159-5' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="74159-5" Work Schedule (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-131).</sch:assert>
      <sch:assert id="a-3349-132" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-132).</sch:assert>
      <sch:assert id="a-3349-133" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-133).</sch:assert>
      <sch:assert id="a-3349-134" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Work Schedule ODH urn:oid:2.16.840.1.113883.1.11.20561 DYNAMIC (CONF:3349-134).</sch:assert>
      <sch:assert id="a-3349-217" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-217).</sch:assert>
      <sch:assert id="a-3349-218" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-218).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-135-branch-135-errors-abstract" abstract="true">
      <sch:assert id="a-3349-139-branch-135" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3349-139).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-135-branch-135-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223' and @extension='2017-11-30']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-135-branch-135-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-137-branch-137-errors-abstract" abstract="true">
      <sch:assert id="a-3349-220-branch-137" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3349-220).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-137-branch-137-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223' and @extension='2017-11-30']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-137-branch-137-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-180" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.210'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-180) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.210" (CONF:3349-185). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-186).</sch:assert>
      <sch:assert id="a-3349-181" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-181).</sch:assert>
      <sch:assert id="a-3349-182" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Work Classification ODH urn:oid:2.16.840.1.113883.1.11.20560 DYNAMIC (CONF:3349-182).</sch:assert>
      <sch:assert id="a-3349-183" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-183).</sch:assert>
      <sch:assert id="a-3349-184" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-184).</sch:assert>
      <sch:assert id="a-3349-187" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-187).</sch:assert>
      <sch:assert id="a-3349-188" test="cda:code[@code='85104-8' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="85104-8" Compensation and Sector Employment Type (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-188).</sch:assert>
      <sch:assert id="a-3349-189" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:3349-189).</sch:assert>
      <sch:assert id="a-3349-190" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-190).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.210' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-errors-abstract" abstract="true">
      <sch:assert id="a-3349-270" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.224'][@extension='2017-11-30'])=1">SHALL contain exactly one [1..1] templateId (CONF:3349-270) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.224" (CONF:3349-274). SHALL contain exactly one [1..1] @extension="2017-11-30" (CONF:3349-275).</sch:assert>
      <sch:assert id="a-3349-271" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3349-271).</sch:assert>
      <sch:assert id="a-3349-272" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3349-272).</sch:assert>
      <sch:assert id="a-3349-273" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3349-273).</sch:assert>
      <sch:assert id="a-3349-276" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3349-276).</sch:assert>
      <sch:assert id="a-3349-277" test="cda:code[@code='87707-6' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="87707-6" Job supervisory level or pay grade (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-277).</sch:assert>
      <sch:assert id="a-3349-278" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3349-278).</sch:assert>
      <sch:assert id="a-3349-279" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3349-279).</sch:assert>
      <sch:assert id="a-3349-280" test="count(cda:value)=1">SHALL contain exactly one [1..1] value, which SHOULD be selected from ValueSet Job Supervisory Level or Pay Grade (ODH) urn:oid:2.16.840.1.114222.4.11.7613 DYNAMIC (CONF:3349-280).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.224' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-1" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.293' and @extension='2018-04-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entry (CONF:3368-1) such that it SHALL contain exactly one [1..1] Pregnancy Observation (SUPPLEMENTAL PREGNANCY) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.293:2018-04-01) (CONF:3368-26530).</sch:assert>
      <sch:assert id="a-3368-3" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.2.80'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-3) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.2.80" (CONF:3368-9). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-10).</sch:assert>
      <sch:assert id="a-3368-4" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-4).</sch:assert>
      <sch:assert id="a-3368-11" test="cda:code[@code='90767-5']">This code SHALL contain exactly one [1..1] @code="90767-5" Pregnancy summary Document (CONF:3368-11).</sch:assert>
      <sch:assert id="a-3368-12" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-12).</sch:assert>
      <sch:assert id="a-3368-13" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:3368-13).</sch:assert>
      <sch:assert id="a-3368-14" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:3368-14).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.80' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26533" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26533).</sch:assert>
      <sch:assert id="a-3368-26535" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Estimated Gestational Age Code Including Method urn:oid:2.16.840.1.113883.11.20.9.82 DYNAMIC (CONF:3368-26535).</sch:assert>
      <sch:assert id="a-3368-26536" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:3368-26536).</sch:assert>
      <sch:assert id="a-3368-26541" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26541).</sch:assert>
      <sch:assert id="a-3368-26542" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26542).</sch:assert>
      <sch:assert id="a-3368-26543" test="count(cda:value[@xsi:type='PQ'])=1">SHALL contain exactly one [1..1] value with @xsi:type="PQ" (CONF:3368-26543).</sch:assert>
      <sch:assert id="a-3368-26820" test="cda:value[@xsi:type='PQ'][@unit='d']">This value SHALL contain exactly one [1..1] @unit="d" days (CodeSystem: UCUM urn:oid:2.16.840.1.113883.6.8) (CONF:3368-26820).</sch:assert>
      <sch:assert id="a-3368-26977" test="not(cda:entryRelationship) or cda:entryRelationship[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.122']])=1]">The entryRelationship, if present, SHALL contain exactly one [1..1] Entry Reference (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.122) (CONF:3368-26977).</sch:assert>
      <sch:assert id="a-3368-26978" test="not(cda:entryRelationship) or cda:entryRelationship[@typeCode='REFR']">The entryRelationship, if present, SHALL contain exactly one [1..1] @typeCode="REFR" Refers to (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3368-26978).</sch:assert>
      <sch:assert id="a-3368-26979-c" test="not(tested)">The Entry Reference template SHALL contain an id that references an Estimated Date of Delivery (SUPPLEMENTAL PREGNANCY) template [urn:hl7ii:2.16.840.1.113883.10.20.22.4.297:2018-04-01] (CONF:3368-26979).</sch:assert>
      <sch:assert id="a-3368-26980" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26980).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.280' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26534" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.280'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26534) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.280" (CONF:3368-26537). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26538).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26549" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.281'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26549) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.281" (CONF:3368-26552). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26553).</sch:assert>
      <sch:assert id="a-3368-26550" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26550).</sch:assert>
      <sch:assert id="a-3368-26551" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26551).</sch:assert>
      <sch:assert id="a-3368-26554" test="cda:code[@code='86645-9']">This code SHALL contain exactly one [1..1] @code="86645-9" Future pregnancy intention Reported (CONF:3368-26554).</sch:assert>
      <sch:assert id="a-3368-26555" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26555).</sch:assert>
      <sch:assert id="a-3368-26556" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26556).</sch:assert>
      <sch:assert id="a-3368-26557" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26557).</sch:assert>
      <sch:assert id="a-3368-26558" test="@moodCode='INT'">SHALL contain exactly one [1..1] @moodCode="INT" Intent (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26558).</sch:assert>
      <sch:assert id="a-3368-26559" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Pregnancy Intention urn:oid:2.16.840.1.113762.1.4.1166.22 DYNAMIC (CONF:3368-26559).</sch:assert>
      <sch:assert id="a-3368-26560" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26560).</sch:assert>
      <sch:assert id="a-3368-26821" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:3368-26821).</sch:assert>
      <sch:assert id="a-3368-26822" test="cda:effectiveTime[count(cda:high)=1]">This effectiveTime SHALL contain exactly one [1..1] high (CONF:3368-26822).</sch:assert>
      <sch:assert id="a-3368-26981" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26981).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.281' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26564" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.282'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26564) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.282" (CONF:3368-26567). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26568).</sch:assert>
      <sch:assert id="a-3368-26565" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26565).</sch:assert>
      <sch:assert id="a-3368-26566" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26566).</sch:assert>
      <sch:assert id="a-3368-26569" test="cda:code[@code='11996-6']">This code SHALL contain exactly one [1..1] @code="11996-6"  [#] Pregnancies (CONF:3368-26569).</sch:assert>
      <sch:assert id="a-3368-26570" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26570).</sch:assert>
      <sch:assert id="a-3368-26571" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26571).</sch:assert>
      <sch:assert id="a-3368-26572" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26572).</sch:assert>
      <sch:assert id="a-3368-26573" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26573).</sch:assert>
      <sch:assert id="a-3368-26574" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26574).</sch:assert>
      <sch:assert id="a-3368-26575" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26575).</sch:assert>
      <sch:assert id="a-3368-26982" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26982).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.282' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26577" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.291'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26577) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.291" (CONF:3368-26580). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26581).</sch:assert>
      <sch:assert id="a-3368-26578" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Other Pregnancy Outcome urn:oid:2.16.840.1.113883.11.20.9.84 DYNAMIC (CONF:3368-26578).</sch:assert>
      <sch:assert id="a-3368-26579" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26579).</sch:assert>
      <sch:assert id="a-3368-26584" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26584).</sch:assert>
      <sch:assert id="a-3368-26585" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26585).</sch:assert>
      <sch:assert id="a-3368-26586" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26586).</sch:assert>
      <sch:assert id="a-3368-26587" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26587).</sch:assert>
      <sch:assert id="a-3368-26588" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26588).</sch:assert>
      <sch:assert id="a-3368-26983" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26983).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.291' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26590" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.284'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26590) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.284" (CONF:3368-26592). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26593).</sch:assert>
      <sch:assert id="a-3368-26591" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26591).</sch:assert>
      <sch:assert id="a-3368-26594" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26594).</sch:assert>
      <sch:assert id="a-3368-26595" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26595).</sch:assert>
      <sch:assert id="a-3368-26596" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26596).</sch:assert>
      <sch:assert id="a-3368-26597" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26597).</sch:assert>
      <sch:assert id="a-3368-26598" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Pregnancy Outcome urn:oid:2.16.840.1.113883.11.20.9.86 DYNAMIC (CONF:3368-26598).</sch:assert>
      <sch:assert id="a-3368-26599" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26599).</sch:assert>
      <sch:assert id="a-3368-26601" test="cda:code[@code='63893-2']">This code SHALL contain exactly one [1..1] @code="63893-2" Outcome of pregnancy (CONF:3368-26601).</sch:assert>
      <sch:assert id="a-3368-26602" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26602).</sch:assert>
      <sch:assert id="a-3368-26892" test="not(cda:entryRelationship) or cda:entryRelationship[count(cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.299' and @extension='2018-04-01']])=1]">The entryRelationship, if present, SHALL contain exactly one [1..1] Method of Delivery (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.299:2018-04-01) (CONF:3368-26892).</sch:assert>
      <sch:assert id="a-3368-26893" test="not(cda:entryRelationship) or cda:entryRelationship[@typeCode='REFR']">The entryRelationship, if present, SHALL contain exactly one [1..1] @typeCode="REFR" Refers to (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:3368-26893).</sch:assert>
      <sch:assert id="a-3368-26984" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26984).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.284' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26603" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.285'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26603) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.285" (CONF:3368-26606). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26607).</sch:assert>
      <sch:assert id="a-3368-26604" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26604).</sch:assert>
      <sch:assert id="a-3368-26605" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26605).</sch:assert>
      <sch:assert id="a-3368-26608" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26608).</sch:assert>
      <sch:assert id="a-3368-26609" test="cda:code[@code='249197004']">This code SHALL contain exactly one [1..1] @code="249197004" Maternal condition during puerperium (observable entity) (CONF:3368-26609).</sch:assert>
      <sch:assert id="a-3368-26610" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:3368-26610).</sch:assert>
      <sch:assert id="a-3368-26611" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26611).</sch:assert>
      <sch:assert id="a-3368-26612" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26612).</sch:assert>
      <sch:assert id="a-3368-26613" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Postpartum Status urn:oid:2.16.840.1.113883.11.20.9.87 DYNAMIC (CONF:3368-26613).</sch:assert>
      <sch:assert id="a-3368-26614" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26614).</sch:assert>
      <sch:assert id="a-3368-26985" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26985).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.285' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26631" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.286'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26631) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.286" (CONF:3368-26634). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26635).</sch:assert>
      <sch:assert id="a-3368-26632" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26632).</sch:assert>
      <sch:assert id="a-3368-26633" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26633).</sch:assert>
      <sch:assert id="a-3368-26636" test="cda:code[@code='57722-1']">This code SHALL contain exactly one [1..1] @code="57722-1" Birth plurality of Pregnancy (CONF:3368-26636).</sch:assert>
      <sch:assert id="a-3368-26637" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26637).</sch:assert>
      <sch:assert id="a-3368-26638" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26638).</sch:assert>
      <sch:assert id="a-3368-26639" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26639).</sch:assert>
      <sch:assert id="a-3368-26640" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26640).</sch:assert>
      <sch:assert id="a-3368-26641" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26641).</sch:assert>
      <sch:assert id="a-3368-26642" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26642).</sch:assert>
      <sch:assert id="a-3368-26986" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26986).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.286' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26643" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.292'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26643) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.292" (CONF:3368-26645). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26646).</sch:assert>
      <sch:assert id="a-3368-26644" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26644).</sch:assert>
      <sch:assert id="a-3368-26647" test="cda:code[@code='10162-6']">This code SHALL contain exactly one [1..1] @code="10162-6" History of Pregnancies Narrative (CONF:3368-26647).</sch:assert>
      <sch:assert id="a-3368-26648" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26648).</sch:assert>
      <sch:assert id="a-3368-26722" test="@classCode='CLUSTER'">SHALL contain exactly one [1..1] @classCode="CLUSTER" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26722).</sch:assert>
      <sch:assert id="a-3368-26723" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26723).</sch:assert>
      <sch:assert id="a-3368-26987" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26987).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.292' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26649" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.283'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26649) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.283" (CONF:3368-26652). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26653).</sch:assert>
      <sch:assert id="a-3368-26650" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26650).</sch:assert>
      <sch:assert id="a-3368-26651" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26651).</sch:assert>
      <sch:assert id="a-3368-26654" test="cda:code[@code='11977-6']">This code SHALL contain exactly one [1..1] @code="11977-6"  [#] Parity (CONF:3368-26654).</sch:assert>
      <sch:assert id="a-3368-26655" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26655).</sch:assert>
      <sch:assert id="a-3368-26656" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26656).</sch:assert>
      <sch:assert id="a-3368-26657" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26657).</sch:assert>
      <sch:assert id="a-3368-26658" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26658).</sch:assert>
      <sch:assert id="a-3368-26659" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26659).</sch:assert>
      <sch:assert id="a-3368-26660" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26660).</sch:assert>
      <sch:assert id="a-3368-26988" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26988).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.283' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26662" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.287'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26662) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.287" (CONF:3368-26665). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26666).</sch:assert>
      <sch:assert id="a-3368-26663" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26663).</sch:assert>
      <sch:assert id="a-3368-26664" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26664).</sch:assert>
      <sch:assert id="a-3368-26667" test="cda:code[@code='11612-9']">This code SHALL contain exactly one [1..1] @code="11612-9" [#] Abortions (CONF:3368-26667).</sch:assert>
      <sch:assert id="a-3368-26668" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26668).</sch:assert>
      <sch:assert id="a-3368-26669" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26669).</sch:assert>
      <sch:assert id="a-3368-26670" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26670).</sch:assert>
      <sch:assert id="a-3368-26671" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26671).</sch:assert>
      <sch:assert id="a-3368-26672" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26672).</sch:assert>
      <sch:assert id="a-3368-26673" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26673).</sch:assert>
      <sch:assert id="a-3368-26989" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26989).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.287' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26674" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.288'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26674) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.288" (CONF:3368-26677). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26678).</sch:assert>
      <sch:assert id="a-3368-26675" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26675).</sch:assert>
      <sch:assert id="a-3368-26676" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26676).</sch:assert>
      <sch:assert id="a-3368-26679" test="cda:code[@code='11639-2']">This code SHALL contain exactly one [1..1] @code="11639-2" [#] Births.term (CONF:3368-26679).</sch:assert>
      <sch:assert id="a-3368-26680" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26680).</sch:assert>
      <sch:assert id="a-3368-26681" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26681).</sch:assert>
      <sch:assert id="a-3368-26682" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26682).</sch:assert>
      <sch:assert id="a-3368-26683" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26683).</sch:assert>
      <sch:assert id="a-3368-26684" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26684).</sch:assert>
      <sch:assert id="a-3368-26685" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26685).</sch:assert>
      <sch:assert id="a-3368-26990" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26990).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.288' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26686" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.289'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26686) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.289" (CONF:3368-26689). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26690).</sch:assert>
      <sch:assert id="a-3368-26687" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26687).</sch:assert>
      <sch:assert id="a-3368-26688" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26688).</sch:assert>
      <sch:assert id="a-3368-26691" test="cda:code[@code='11637-6']">This code SHALL contain exactly one [1..1] @code="11637-6" [#] Births.preterm (CONF:3368-26691).</sch:assert>
      <sch:assert id="a-3368-26692" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26692).</sch:assert>
      <sch:assert id="a-3368-26693" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26693).</sch:assert>
      <sch:assert id="a-3368-26694" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26694).</sch:assert>
      <sch:assert id="a-3368-26695" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26695).</sch:assert>
      <sch:assert id="a-3368-26696" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26696).</sch:assert>
      <sch:assert id="a-3368-26697" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26697).</sch:assert>
      <sch:assert id="a-3368-26991" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26991).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.289' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26698" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.290'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26698) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.290" (CONF:3368-26701). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26702).</sch:assert>
      <sch:assert id="a-3368-26699" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26699).</sch:assert>
      <sch:assert id="a-3368-26700" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26700).</sch:assert>
      <sch:assert id="a-3368-26703" test="cda:code[@code='11638-4']">This code SHALL contain exactly one [1..1] @code="11638-4" [#] Births.still living (CONF:3368-26703).</sch:assert>
      <sch:assert id="a-3368-26704" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26704).</sch:assert>
      <sch:assert id="a-3368-26705" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26705).</sch:assert>
      <sch:assert id="a-3368-26706" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:3368-26706).</sch:assert>
      <sch:assert id="a-3368-26707" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:3368-26707).</sch:assert>
      <sch:assert id="a-3368-26708" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26708).</sch:assert>
      <sch:assert id="a-3368-26709" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26709).</sch:assert>
      <sch:assert id="a-3368-26992" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26992).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.290' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26726" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26726).</sch:assert>
      <sch:assert id="a-3368-26728" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26728).</sch:assert>
      <sch:assert id="a-3368-26729" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:3368-26729).</sch:assert>
      <sch:assert id="a-3368-26732" test="cda:code[@code='68499-3']">This code SHALL contain exactly one [1..1] @code="68499-3" Date of last live birth (CONF:3368-26732).</sch:assert>
      <sch:assert id="a-3368-26733" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26733).</sch:assert>
      <sch:assert id="a-3368-26734" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26734).</sch:assert>
      <sch:assert id="a-3368-26735" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26735).</sch:assert>
      <sch:assert id="a-3368-26736" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:3368-26736).</sch:assert>
      <sch:assert id="a-3368-26993" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26993).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.294' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26727" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.294'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26727) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.294" (CONF:3368-26730). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26731).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26740" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26740).</sch:assert>
      <sch:assert id="a-3368-26742" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26742).</sch:assert>
      <sch:assert id="a-3368-26743" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:3368-26743).</sch:assert>
      <sch:assert id="a-3368-26746" test="cda:code[@code='69044-6']">This code SHALL contain exactly one [1..1] @code="69044-6" Date of first prenatal care visit (CONF:3368-26746).</sch:assert>
      <sch:assert id="a-3368-26747" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26747).</sch:assert>
      <sch:assert id="a-3368-26748" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26748).</sch:assert>
      <sch:assert id="a-3368-26749" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26749).</sch:assert>
      <sch:assert id="a-3368-26750" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:3368-26750).</sch:assert>
      <sch:assert id="a-3368-26994" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26994).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.295' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26741" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.295'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26741) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.295" (CONF:3368-26744). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26745).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26752" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26752).</sch:assert>
      <sch:assert id="a-3368-26754" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26754).</sch:assert>
      <sch:assert id="a-3368-26755" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:3368-26755).</sch:assert>
      <sch:assert id="a-3368-26758" test="cda:code[@code='68493-6']">This code SHALL contain exactly one [1..1] @code="68493-6" Total number of prenatal visits for this pregnancy (CONF:3368-26758).</sch:assert>
      <sch:assert id="a-3368-26759" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26759).</sch:assert>
      <sch:assert id="a-3368-26760" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26760).</sch:assert>
      <sch:assert id="a-3368-26761" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26761).</sch:assert>
      <sch:assert id="a-3368-26762" test="count(cda:value[@xsi:type='INT'])=1">SHALL contain exactly one [1..1] value with @xsi:type="INT" (CONF:3368-26762).</sch:assert>
      <sch:assert id="a-3368-26995" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26995).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.296' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26753" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.296'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26753) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.296" (CONF:3368-26756). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26757).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-errors-abstract" />
      <sch:assert id="a-3368-26797" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26797).</sch:assert>
      <sch:assert id="a-3368-26798" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26798).</sch:assert>
      <sch:assert id="a-3368-26807" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:3368-26807).</sch:assert>
      <sch:assert id="a-3368-26996" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:3368-26996).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.293' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26775" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.293'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26775) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.293" (CONF:3368-26795). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26796).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-26776-branch-26776-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26802-branch-26776" test="not(cda:time) or cda:time[@value]">This time SHALL contain exactly one [1..1] @value (CONF:3368-26802).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-26776-branch-26776-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.293' and @extension='2018-04-01']]/cda:performer[cda:time]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-26776-branch-26776-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-3368-26811" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26811).</sch:assert>
      <sch:assert id="a-3368-26812" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26812).</sch:assert>
      <sch:assert id="a-3368-26814" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Estimated Date of Delivery Including Method urn:oid:2.16.840.1.113883.11.20.9.81 DYNAMIC (CONF:3368-26814).</sch:assert>
      <sch:assert id="a-3368-26815" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:3368-26815).</sch:assert>
      <sch:assert id="a-3368-26816" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:3368-26816).</sch:assert>
      <sch:assert id="a-3368-26817" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:3368-26817).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.297' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26808" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.297'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26808) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.297" (CONF:3368-26809). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26810).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-3378-326" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" Procedure (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-326).</sch:assert>
      <sch:assert id="a-3378-327" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-327).</sch:assert>
      <sch:assert id="a-3378-328" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-328).</sch:assert>
      <sch:assert id="a-3378-453" test="cda:code[@code='17636008']">This code SHALL contain exactly one [1..1] @code="17636008" Specimen collection (procedure) (CONF:3378-453).</sch:assert>
      <sch:assert id="a-3378-454" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:3378-454).</sch:assert>
      <sch:assert id="a-3378-33112" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.415'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-33112) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.415" (CONF:3378-33113). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-33114).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.415' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-errors-abstract" abstract="true">
      <sch:assert id="a-3378-371" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.418'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-371) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.418" (CONF:3378-372). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-373).</sch:assert>
      <sch:assert id="a-3378-374-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Result Status urn:oid:2.16.840.1.113883.21.55 DYNAMIC (CONF:3378-374).</sch:assert>
      <sch:assert id="a-3378-375" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-375).</sch:assert>
      <sch:assert id="a-3378-376" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-376).</sch:assert>
      <sch:assert id="a-3378-377" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-377).</sch:assert>
      <sch:assert id="a-3378-510" test="cda:code[@code='92235-1']">This code SHALL contain exactly one [1..1] @code="92235-1" Lab order result status (CONF:3378-510).</sch:assert>
      <sch:assert id="a-3378-511" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3378-511).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.418' and @extension='2018-06-11']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-3378-378" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.419'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-378) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.419" (CONF:3378-379). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-380).</sch:assert>
      <sch:assert id="a-3378-381" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Observation Result Status Codes Interpretation urn:oid:2.16.840.1.113883.21.38 DYNAMIC (CONF:3378-381).</sch:assert>
      <sch:assert id="a-3378-382" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-382).</sch:assert>
      <sch:assert id="a-3378-383" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-383).</sch:assert>
      <sch:assert id="a-3378-384" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-384).</sch:assert>
      <sch:assert id="a-3378-486" test="cda:code[@code='92236-9']">This code SHALL contain exactly one [1..1] @code="92236-9" Lab observation result status (CONF:3378-486).</sch:assert>
      <sch:assert id="a-3378-487" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3378-487).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.419' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-3378-393" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:3378-393).</sch:assert>
      <sch:assert id="a-3378-394" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:3378-394).</sch:assert>
      <sch:assert id="a-3378-418" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-418).</sch:assert>
      <sch:assert id="a-3378-419" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-419).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.417' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors-abstract" />
      <sch:assert id="a-3378-392" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.417'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-392) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.417" (CONF:3378-408). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-409).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors-abstract" />
      <sch:assert id="a-3378-433" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Specimen Reject Reason urn:oid:2.16.840.1.113883.21.330 DYNAMIC (CONF:3378-433).</sch:assert>
      <sch:assert id="a-3378-436" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:3378-436).</sch:assert>
      <sch:assert id="a-3378-440" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-440).</sch:assert>
      <sch:assert id="a-3378-441" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-441).</sch:assert>
      <sch:assert id="a-3378-512" test="cda:code[@code='93048-7']">This code SHALL contain exactly one [1..1] @code="93048-7" Reason for specimen rejection (CONF:3378-512).</sch:assert>
      <sch:assert id="a-3378-513" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3378-513).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.420' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-errors-abstract" />
      <sch:assert id="a-3378-432" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.420'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-432) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.420" (CONF:3378-434). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-435).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-errors-abstract" />
      <sch:assert id="a-3378-443" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Specimen Condition urn:oid:2.16.840.1.113883.21.333 DYNAMIC (CONF:3378-443).</sch:assert>
      <sch:assert id="a-3378-449" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-449).</sch:assert>
      <sch:assert id="a-3378-450" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3378-450).</sch:assert>
      <sch:assert id="a-3378-451" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3378-451).</sch:assert>
      <sch:assert id="a-3378-514" test="cda:code[@code='93047-9']">This code SHALL contain exactly one [1..1] @code="93047-9" Specimen condition (CONF:3378-514).</sch:assert>
      <sch:assert id="a-3378-515" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3378-515).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.421' and @extension='2018-06-12']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-errors-abstract" />
      <sch:assert id="a-3378-442" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.421'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-442) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.421" (CONF:3378-444). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-445).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-3378-455" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.410'][@extension='2018-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3378-455) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.410" (CONF:3378-458). SHALL contain exactly one [1..1] @extension="2018-09-01" (CONF:3378-459).</sch:assert>
      <sch:assert id="a-3378-456" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:3378-456).</sch:assert>
      <sch:assert id="a-3378-457" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:3378-457).</sch:assert>
      <sch:assert id="a-3378-460" test="cda:participantRole/cda:playingEntity[count(cda:code)=1]">This playingEntity SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet HL7 Specimen Type urn:oid:2.16.840.1.113883.21.327 DYNAMIC (CONF:3378-460).</sch:assert>
      <sch:assert id="a-3378-461" test="cda:participantRole[@classCode]">This participantRole SHALL contain exactly one [1..1] @classCode, which SHALL be selected from ValueSet RoleClassSpecimen urn:oid:2.16.840.1.113883.1.11.11591 DYNAMIC (CONF:3378-461).</sch:assert>
      <sch:assert id="a-3378-462" test="cda:participantRole[count(cda:id)=1]">This participantRole SHALL contain exactly one [1..1] id (CONF:3378-462).</sch:assert>
      <sch:assert id="a-3378-463" test="@typeCode='PRD'">SHALL contain exactly one [1..1] @typeCode="PRD" product (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:3378-463).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.410' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-3368-26825" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.298'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26825) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.298" (CONF:3368-26840). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26841).</sch:assert>
      <sch:assert id="a-3368-26828-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Pregnancy Related Findings urn:oid:2.16.840.1.113883.11.20.9.88 DYNAMIC (CONF:3368-26828).</sch:assert>
      <sch:assert id="a-3368-26851" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26851).</sch:assert>
      <sch:assert id="a-3368-26852" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26852).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.298' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors-abstract" />
      <sch:assert id="a-3368-26858" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.299'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26858) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.299" (CONF:3368-26889). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26890).</sch:assert>
      <sch:assert id="a-3368-26859" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Delivery (NCHS) urn:oid:1.3.6.1.4.1.19376.1.7.3.1.1.13.8.14 DYNAMIC (CONF:3368-26859).</sch:assert>
      <sch:assert id="a-3368-26887" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" Procedure (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26887).</sch:assert>
      <sch:assert id="a-3368-26888" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26888).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.299' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors-abstract" />
      <sch:assert id="a-3368-26897" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26897).</sch:assert>
      <sch:assert id="a-3368-26899" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet D(Rh) Type urn:oid:2.16.840.1.113883.11.20.9.89 DYNAMIC (CONF:3368-26899).</sch:assert>
      <sch:assert id="a-3368-26914" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26914).</sch:assert>
      <sch:assert id="a-3368-26915" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26915).</sch:assert>
      <sch:assert id="a-3368-26916" test="cda:code[@code='10331-7']">This code SHALL contain exactly one [1..1] @code="10331-7" Rh [Type] in Blood (CONF:3368-26916).</sch:assert>
      <sch:assert id="a-3368-26917" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26917).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.300' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26896" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.300'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26896) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.300" (CONF:3368-26904). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26905).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-3368-26918" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.298'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26918) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.298" (CONF:3368-26919). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26920).</sch:assert>
      <sch:assert id="a-3368-26921-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet D(Rh) Sensitized urn:oid:2.16.840.1.113883.11.20.9.90 DYNAMIC (CONF:3368-26921).</sch:assert>
      <sch:assert id="a-3368-26922" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26922).</sch:assert>
      <sch:assert id="a-3368-26923" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26923).</sch:assert>
      <sch:assert id="a-3368-26924" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3368-26924).</sch:assert>
      <sch:assert id="a-3368-26925" test="cda:code[count(cda:translation)=1]">This code SHALL contain exactly one [1..1] translation (CONF:3368-26925).</sch:assert>
      <sch:assert id="a-3368-26926" test="cda:code[@code='55607006']">This code SHALL contain exactly one [1..1] @code="55607006" Problem (CONF:3368-26926).</sch:assert>
      <sch:assert id="a-3368-26927" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:3368-26927).</sch:assert>
      <sch:assert id="a-3368-26928" test="cda:code/cda:translation[@code='75326-9']">This translation SHALL contain exactly one [1..1] @code="75326-9" Problem (CONF:3368-26928).</sch:assert>
      <sch:assert id="a-3368-26929" test="cda:code/cda:translation[@codeSystem='2.16.840.1.113883.6.1']">This translation SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:3368-26929).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.302' and @extension='2018-08-31']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-errors-abstract" />
      <sch:assert id="a-3368-26934" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.302'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26934) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.302" (CONF:3368-26966). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26967).</sch:assert>
      <sch:assert id="a-3368-26941" test="count(cda:consumable)=1">SHALL contain exactly one [1..1] consumable (CONF:3368-26941).</sch:assert>
      <sch:assert id="a-3368-26942" test="cda:consumable[count(cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.303' and @extension='2018-04-01']])=1]">This consumable SHALL contain exactly one [1..1] D Immune Globulin (RhIG) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.303:2018-04-01) (CONF:3368-26942).</sch:assert>
      <sch:assert id="a-3368-26964" test="@classCode='SBADM'">SHALL contain exactly one [1..1] @classCode="SBADM" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:3368-26964).</sch:assert>
      <sch:assert id="a-3368-26965" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:3368-26965).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-errors" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.302' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors-abstract" />
      <sch:assert id="a-3368-26969" test="count(cda:manufacturedMaterial)=1">SHALL contain exactly one [1..1] manufacturedMaterial (CONF:3368-26969).</sch:assert>
      <sch:assert id="a-3368-26970-c" test="cda:manufacturedMaterial[count(cda:code[@codeSystem='2.16.840.1.113883.6.88'])=1]">This manufacturedMaterial SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Rho(D) Immune Globulin urn:oid:2.16.840.1.113883.11.20.9.91 DYNAMIC (CONF:3368-26970).</sch:assert>
      <sch:assert id="a-3368-26971" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:3368-26971).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-errors" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.303' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-errors-abstract" />
      <sch:assert id="a-3368-26968" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.303'][@extension='2018-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:3368-26968) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.303" (CONF:3368-26972). SHALL contain exactly one [1..1] @extension="2018-04-01" (CONF:3368-26973).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-errors-abstract" />
      <sch:assert id="a-4411-271" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4411-271).</sch:assert>
      <sch:assert id="a-4411-273" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:4411-273).</sch:assert>
      <sch:assert id="a-4411-302-c" test="not(tested)">If value data type is CD (xsi:type="CD") and the contained value/@code is an RCTC trigger code, value **SHALL** contain @sdtc:valueSet='2.16.840.1.114222.4.11.7508' and sdtc:valueSetVersion (RCTC Definition Version used (e.g. 2020-11-13))
If value data type is CD (xsi:type="CD") and the contained value/translation/@code is an RCTC trigger code, value/translation **SHALL** contain @sdtc:valueSet='2.16.840.1.114222.4.11.7508' and value/translation/sdtc:valueSetVersion (RCTC Definition Version used (e.g. 2020-11-13)) (CONF:4411-302).</sch:assert>
      <sch:assert id="a-4411-298" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4411-298).</sch:assert>
      <sch:assert id="a-4411-301-c" test="cda:code[@sdtc:valueSet and @sdtc:valueSetVersion] or cda:code[ not(@sdtc:valueSet or @sdtc:valueSetVersion)]">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-301).</sch:assert>
      <sch:assert id="a-4411-304-c" test="cda:value[@sdtc:valueSet and @sdtc:valueSetVersion] or cda:value[ not(@sdtc:valueSet or @sdtc:valueSetVersion)]">If either value[xsi:type="CD"]/@sdtc:valueSet or value[xsi:type="CD"]/@sdtc:valueSetVersion is present then both value[xsi:type="CD"]/@sdtc:valueSet and value[xsi:type="CD"]/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-304).</sch:assert>
      <sch:assert id="a-4411-288" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4411-288).</sch:assert>
      <sch:assert id="a-4411-289" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4411-289).</sch:assert>
      <sch:assert id="a-4411-299" test="cda:statusCode[@code and @code=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.10.20.15.2.5.1']/voc:code/@value]">This statusCode SHALL contain exactly one [1..1] @code, which SHALL be selected from ValueSet Initial Case Report Trigger Code Result Status urn:oid:2.16.840.1.113883.10.20.15.2.5.1 STATIC 2016-11-01 (CONF:4411-299).</sch:assert>
      <sch:assert id="a-4411-300-c" test="cda:code[@sdtc:valueSet and @sdtc:valueSetVersion] or cda:value[@sdtc:valueSet and @sdtc:valueSetVersion]">At least one of (code/@sdtc:valueSet and code/@sdtc:valueSetVersion) or (value/@sdtc:valueSet and value/@sdtc:valueSetVersion) **SHALL** be present (CONF:4411-300).</sch:assert>
      <sch:assert id="a-4411-307" test="not(cda:reference) or cda:reference[count(cda:externalDocument)=1]">The reference, if present, SHALL contain exactly one [1..1] externalDocument (CONF:4411-307).</sch:assert>
      <sch:assert id="a-4411-308" test="not(cda:reference/cda:externalDocument) or cda:reference/cda:externalDocument[count(cda:setId)=1]">This externalDocument SHALL contain exactly one [1..1] setId (CONF:4411-308).</sch:assert>
      <sch:assert id="a-4411-309" test="not(cda:reference/cda:externalDocument) or cda:reference/cda:externalDocument[count(cda:versionNumber)=1]">This externalDocument SHALL contain exactly one [1..1] versionNumber (CONF:4411-309).</sch:assert>
      <sch:assert id="a-4411-532-c" test="(cda:code/cda:translation/@sdtc:valueSet and cda:code/cda:translation/@sdtc:valueSetVersion) or (not(cda:code/cda:translation/@sdtc:valueSet) and not(cda:code/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-532).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.2' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-errors-abstract" />
      <sch:assert id="a-4411-270" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.2'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-270) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.2" (CONF:4411-278). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-279).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-4411-422" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4411-422).</sch:assert>
      <sch:assert id="a-4411-433" test="@classCode">SHALL contain exactly one [1..1] @classCode (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4411-433).</sch:assert>
      <sch:assert id="a-4411-434" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4411-434).</sch:assert>
      <sch:assert id="a-4411-438" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Lab Obs Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1057 DYNAMIC (CONF:4411-438).</sch:assert>
      <sch:assert id="a-4411-541-c" test="(cda:code/@sdtc:valueSet and cda:code/@sdtc:valueSetVersion) or (not(cda:code/@sdtc:valueSet) and not(cda:code/@sdtc:valueSetVersion))">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-541).</sch:assert>
      <sch:assert id="a-4411-542-c" test="(cda:code/cda:translation/@sdtc:valueSet and cda:code/cda:translation/@sdtc:valueSetVersion) or (not(cda:code/cda:translation/@sdtc:valueSet) and not(cda:code/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-542).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.35' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-errors-abstract" />
      <sch:assert id="a-4411-435" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.35'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-435) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.35" (CONF:4411-436). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-437).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-4420-1220" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4420-1220).</sch:assert>
      <sch:assert id="a-4420-1221" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.34.3.45'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4420-1221) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.34.3.45" (CONF:4420-1225). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4420-1226).</sch:assert>
      <sch:assert id="a-4420-1222" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4420-1222).</sch:assert>
      <sch:assert id="a-4420-1223" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Gender Identity urn:oid:2.16.840.1.113762.1.4.1021.32 DYNAMIC (CONF:4420-1223).</sch:assert>
      <sch:assert id="a-4420-1224" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:4420-1224).</sch:assert>
      <sch:assert id="a-4420-1227" test="cda:code[@code='76691-5']">This code SHALL contain exactly one [1..1] @code=" 76691-5" Gender identity (CONF:4420-1227).</sch:assert>
      <sch:assert id="a-4420-1228" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:4420-1228).</sch:assert>
      <sch:assert id="a-4420-1229-c" test="not(tested)">If value/@code not from value set ONC Administrative Sex urn:oid:2.16.840.1.113762.1.4.1 STATIC 2016-06-01, then value/@nullFlavor SHALL be “OTH” (CONF:4420-1229).</sch:assert>
      <sch:assert id="a-4420-1230" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4420-1230).</sch:assert>
      <sch:assert id="a-4420-1231" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4420-1231).</sch:assert>
      <sch:assert id="a-4420-1269" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4420-1269).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.34.3.45' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors-abstract" />
      <sch:assert id="a-4411-311" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.4'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-311) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.4" (CONF:4411-319). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-320).</sch:assert>
      <sch:assert id="a-4411-325-c" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:4411-325).</sch:assert>
      <sch:assert id="a-4411-317" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4411-317).</sch:assert>
      <sch:assert id="a-4411-318" test="@moodCode='RQO'">SHALL contain exactly one [1..1] @moodCode="RQO" Request (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4411-318).</sch:assert>
      <sch:assert id="a-4411-470" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Lab Order Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1056 DYNAMIC (CONF:4411-470).</sch:assert>
      <sch:assert id="a-4411-473" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Lab Order Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1056 DYNAMIC (CONF:4411-473).</sch:assert>
      <sch:assert id="a-4411-533-c" test="(cda:code/cda:translation/@sdtc:valueSet and cda:code/cda:translation/@sdtc:valueSetVersion) or (not(cda:code/cda:translation/@sdtc:valueSet) and not(cda:code/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-533).</sch:assert>
      <sch:assert id="a-4411-534-c" test="(cda:code/@sdtc:valueSet and cda:code/@sdtc:valueSetVersion) or (not(cda:code/@sdtc:valueSet) and not(cda:code/@sdtc:valueSetVersion))">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-534).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.4' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-errors-abstract" />
      <sch:assert id="a-4411-521" test="count(cda:manufacturedMaterial)=1">SHALL contain exactly one [1..1] manufacturedMaterial (CONF:4411-521).</sch:assert>
      <sch:assert id="a-4411-522" test="cda:manufacturedMaterial[count(cda:code)=1]">This manufacturedMaterial SHALL contain exactly one [1..1] code (CONF:4411-522).</sch:assert>
      <sch:assert id="a-4411-523" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:4411-523).</sch:assert>
      <sch:assert id="a-4411-537" test="cda:manufacturedMaterial/cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-537).</sch:assert>
      <sch:assert id="a-4411-538" test="not(cda:manufacturedMaterial/cda:code/cda:translation) or cda:manufacturedMaterial/cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-538).</sch:assert>
      <sch:assert id="a-4411-539-c" test="(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSet and cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSetVersion) or (not(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSet) and not(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-539).</sch:assert>
      <sch:assert id="a-4411-540-c" test="(cda:manufacturedMaterial/cda:code/@sdtc:valueSet and cda:manufacturedMaterial/cda:code/@sdtc:valueSetVersion) or (not(cda:manufacturedMaterial/cda:code/@sdtc:valueSet) and not(cda:manufacturedMaterial/cda:code/@sdtc:valueSetVersion))">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-540).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-errors" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.36' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-errors-abstract" />
      <sch:assert id="a-4411-520" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.36'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-520) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.36" (CONF:4411-524). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-525).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-errors-abstract" abstract="true">
      <sch:assert id="a-1198-19162" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1198-19162).</sch:assert>
      <sch:assert id="a-1198-7364" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:1198-7364).</sch:assert>
      <sch:assert id="a-1198-7357" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:1198-7357).</sch:assert>
      <sch:assert id="a-1198-7358" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:1198-7358).</sch:assert>
      <sch:assert id="a-1198-19163" test="cda:code[@code='33999-4' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="33999-4" Status (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:1198-19163).</sch:assert>
      <sch:assert id="a-1198-19113" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14 STATIC) (CONF:1198-19113).</sch:assert>
      <sch:assert id="a-1198-7365" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Problem Status urn:oid:2.16.840.1.113883.3.88.12.80.68 DYNAMIC (CONF:1198-7365).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.6' and @extension='2019-06-20']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-errors-abstract" />
      <sch:assert id="a-1198-7359" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.6'][@extension='2019-06-20'])=1">SHALL contain exactly one [1..1] templateId (CONF:1198-7359) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.6" (CONF:1198-10518). SHALL contain exactly one [1..1] @extension="2019-06-20" (CONF:1198-32961).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-errors-abstract" abstract="true">
      <sch:assert id="a-4411-549" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.37'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-549) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.37" (CONF:4411-561). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-571).</sch:assert>
      <sch:assert id="a-4411-559" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4411-559).</sch:assert>
      <sch:assert id="a-4411-560" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4411-560).</sch:assert>
      <sch:assert id="a-4411-562" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4411-562).</sch:assert>
      <sch:assert id="a-4411-569" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4411-569).</sch:assert>
      <sch:assert id="a-4411-572" test="cda:code[@code='67540-5']">This code SHALL contain exactly one [1..1] @code="67540-5" Response to medication (CONF:4411-572).</sch:assert>
      <sch:assert id="a-4411-573" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4411-573).</sch:assert>
      <sch:assert id="a-4411-574" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4411-574).</sch:assert>
      <sch:assert id="a-4411-575" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4411-575).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.37' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-4411-340" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.5'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-340) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.5" (CONF:4411-352). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-353).</sch:assert>
      <sch:assert id="a-4411-343-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD" (CONF:4411-343).</sch:assert>
      <sch:assert id="a-4411-366" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4411-366).</sch:assert>
      <sch:assert id="a-4411-367" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4411-367).</sch:assert>
      <sch:assert id="a-4411-578-c" test="(cda:value/@nullFlavor and cda:value/cda:originalText) or (cda:value/@code and cda:value/@codeSystem)">Either @nullFlavor and originalText SHALL be present or @code and @codeSystem SHALL be present (CONF:4411-578).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.5' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-errors-abstract" />
      <sch:assert id="a-4411-580" test="count(cda:manufacturedMaterial)=1">SHALL contain exactly one [1..1] manufacturedMaterial (CONF:4411-580).</sch:assert>
      <sch:assert id="a-4411-581" test="cda:manufacturedMaterial[count(cda:code)=1]">This manufacturedMaterial SHALL contain exactly one [1..1] code (CONF:4411-581).</sch:assert>
      <sch:assert id="a-4411-587" test="not(cda:manufacturedMaterial/cda:code/cda:translation) or cda:manufacturedMaterial/cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-587).</sch:assert>
      <sch:assert id="a-4411-588-c" test="(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSet and cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSetVersion) or (not(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSet) and not(cda:manufacturedMaterial/cda:code/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-588).</sch:assert>
      <sch:assert id="a-4411-591" test="cda:manufacturedMaterial/cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-591).</sch:assert>
      <sch:assert id="a-4411-592-c" test="(cda:manufacturedMaterial/cda:code/@sdtc:valueSet and cda:manufacturedMaterial/cda:code/@sdtc:valueSetVersion) or (not(cda:manufacturedMaterial/cda:code/@sdtc:valueSet) and not(cda:manufacturedMaterial/cda:code/@sdtc:valueSetVersion))">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4411-592).</sch:assert>
      <sch:assert id="a-4411-593" test="@classCode='MANU'">SHALL contain exactly one [1..1] @classCode="MANU" (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110 STATIC) (CONF:4411-593).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-errors" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.38' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-errors-abstract" />
      <sch:assert id="a-4411-579" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.38'][@extension='2019-04-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4411-579) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.38" (CONF:4411-583). SHALL contain exactly one [1..1] @extension="2019-04-01" (CONF:4411-584).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-4480-37" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.219'][@extension='2020-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4480-37) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.219" (CONF:4480-39). SHALL contain exactly one [1..1] @extension="2020-09-01" (CONF:4480-40).</sch:assert>
      <sch:assert id="a-4480-38" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4480-38).</sch:assert>
      <sch:assert id="a-4480-42" test="cda:code[@code='21844-6' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="21844-6" History of Usual Industry (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4480-42).</sch:assert>
      <sch:assert id="a-4480-43" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:4480-43).</sch:assert>
      <sch:assert id="a-4480-41" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4480-41).</sch:assert>
      <sch:assert id="a-4480-44" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:4480-44).</sch:assert>
      <sch:assert id="a-4480-45" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Industry CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7187 DYNAMIC (CONF:4480-45).</sch:assert>
      <sch:assert id="a-4480-201" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4480-201).</sch:assert>
      <sch:assert id="a-4480-202" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4480-202).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.219' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-4480-23" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221'][@extension='2020-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4480-23) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.221" (CONF:4480-26). SHALL contain exactly one [1..1] @extension="2020-09-01" (CONF:4480-27).</sch:assert>
      <sch:assert id="a-4480-24" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4480-24).</sch:assert>
      <sch:assert id="a-4480-25" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4480-25).</sch:assert>
      <sch:assert id="a-4480-261" test="not(cda:subject) or cda:subject[count(cda:relatedSubject)=1]">The subject, if present, SHALL contain exactly one [1..1] relatedSubject (CONF:4480-261).</sch:assert>
      <sch:assert id="a-4480-29" test="cda:code[@code='21843-8' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="21843-8" History of Usual Occupation (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4480-29).</sch:assert>
      <sch:assert id="a-4480-30" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:4480-30).</sch:assert>
      <sch:assert id="a-4480-32" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:4480-32).</sch:assert>
      <sch:assert id="a-4480-28" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4480-28).</sch:assert>
      <sch:assert id="a-4480-31" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:4480-31).</sch:assert>
      <sch:assert id="a-4480-34" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Occupation CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7186 DYNAMIC (CONF:4480-34).</sch:assert>
      <sch:assert id="a-4480-206" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4480-206).</sch:assert>
      <sch:assert id="a-4480-207" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4480-207).</sch:assert>
      <sch:assert id="a-4480-262" test="not(cda:subject/cda:relatedSubject) or cda:subject/cda:relatedSubject[count(cda:code)=1]">This relatedSubject SHALL contain exactly one [1..1] code (ValueSet: Personal And Legal Relationship Role Type urn:oid:2.16.840.1.113883.11.20.12.1 DYNAMIC) (CONF:4480-262).</sch:assert>
      <sch:assert id="a-4480-264" test="not(cda:subject/cda:relatedSubject) or cda:subject/cda:relatedSubject[@classCode='PRS']">This relatedSubject SHALL contain exactly one [1..1] @classCode="PRS" (CodeSystem: HL7EntityClass urn:oid:2.16.840.1.113883.5.41) (CONF:4480-264).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-36-branch-36-errors-abstract" abstract="true">
      <sch:assert id="a-4480-103-branch-36" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002 STATIC) (CONF:4480-103).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-36-branch-36-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-36-branch-36-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-101-branch-101-errors-abstract" abstract="true">
      <sch:assert id="a-4480-210-branch-101" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4480-210).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-101-branch-101-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-101-branch-101-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-4480-140" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217'][@extension='2020-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4480-140) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.217" (CONF:4480-152). SHALL contain exactly one [1..1] @extension="2020-09-01" (CONF:4480-153).</sch:assert>
      <sch:assert id="a-4480-141" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4480-141).</sch:assert>
      <sch:assert id="a-4480-142" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4480-142).</sch:assert>
      <sch:assert id="a-4480-144" test="count(cda:participant)=1">SHALL contain exactly one [1..1] participant (CONF:4480-144).</sch:assert>
      <sch:assert id="a-4480-145" test="cda:participant[count(cda:participantRole)=1]">This participant SHALL contain exactly one [1..1] participantRole (CONF:4480-145).</sch:assert>
      <sch:assert id="a-4480-234" test="not(cda:subject) or cda:subject[count(cda:relatedSubject)=1]">The subject, if present, SHALL contain exactly one [1..1] relatedSubject (CONF:4480-234).</sch:assert>
      <sch:assert id="a-4480-245" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.216' and @extension='2020-09-01']])=1])=1">SHALL contain exactly one [1..1] entryRelationship (CONF:4480-245) such that it SHALL contain exactly one [1..1] Past or Present Industry Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.216:2020-09-01) (CONF:4480-246).</sch:assert>
      <sch:assert id="a-4480-155" test="cda:code[@code='11341-5' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="11341-5" History of Occupation (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4480-155).</sch:assert>
      <sch:assert id="a-4480-156" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:4480-156).</sch:assert>
      <sch:assert id="a-4480-158" test="cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:4480-158).</sch:assert>
      <sch:assert id="a-4480-162" test="cda:participant[@typeCode='IND']">This participant SHALL contain exactly one [1..1] @typeCode="IND" (CONF:4480-162).</sch:assert>
      <sch:assert id="a-4480-154" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4480-154).</sch:assert>
      <sch:assert id="a-4480-157" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:4480-157).</sch:assert>
      <sch:assert id="a-4480-160" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Occupation CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7186 DYNAMIC (CONF:4480-160).</sch:assert>
      <sch:assert id="a-4480-226" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4480-226).</sch:assert>
      <sch:assert id="a-4480-227" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4480-227).</sch:assert>
      <sch:assert id="a-4480-236" test="not(cda:subject/cda:relatedSubject) or cda:subject/cda:relatedSubject[@classCode='PRS']">This relatedSubject SHALL contain exactly one [1..1] @classCode="PRS" (CodeSystem: HL7EntityClass urn:oid:2.16.840.1.113883.5.41) (CONF:4480-236).</sch:assert>
      <sch:assert id="a-4480-237" test="not(cda:subject/cda:relatedSubject) or cda:subject/cda:relatedSubject[count(cda:code)=1]">This relatedSubject SHALL contain exactly one [1..1] code (ValueSet: Personal And Legal Relationship Role Type urn:oid:2.16.840.1.113883.11.20.12.1 DYNAMIC) (CONF:4480-237).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-146-branch-146-errors-abstract" abstract="true">
      <sch:assert id="a-4480-165-branch-146" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CONF:4480-165).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-146-branch-146-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-146-branch-146-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-245-branch-245-errors-abstract" abstract="true">
      <sch:assert id="a-4480-247-branch-245" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4480-247).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-245-branch-245-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-245-branch-245-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-248-branch-248-errors-abstract" abstract="true">
      <sch:assert id="a-4480-250-branch-248" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4480-250).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-248-branch-248-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-248-branch-248-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-281-branch-281-errors-abstract" abstract="true">
      <sch:assert id="a-4480-283-branch-281" test="@typeCode='REFR'">SHALL contain exactly one [1..1] @typeCode="REFR" (CONF:4480-283).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-281-branch-281-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]/cda:entryRelationship[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-281-branch-281-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-errors-abstract" abstract="true">
      <sch:assert id="a-4480-168" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.216'][@extension='2020-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4480-168) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.216" (CONF:4480-170). SHALL contain exactly one [1..1] @extension="2020-09-01" (CONF:4480-171).</sch:assert>
      <sch:assert id="a-4480-169" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4480-169).</sch:assert>
      <sch:assert id="a-4480-173" test="cda:code[@code='86188-0' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="86188-0" History of Occupation Industry (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4480-173).</sch:assert>
      <sch:assert id="a-4480-174" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" LOINC (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1 STATIC) (CONF:4480-174).</sch:assert>
      <sch:assert id="a-4480-172" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4480-172).</sch:assert>
      <sch:assert id="a-4480-175" test="count(cda:statusCode[@code='completed'])=1">SHALL contain exactly one [1..1] statusCode="completed" Completed (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:4480-175).</sch:assert>
      <sch:assert id="a-4480-176" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Industry CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7187 DYNAMIC (CONF:4480-176).</sch:assert>
      <sch:assert id="a-4480-178" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4480-178).</sch:assert>
      <sch:assert id="a-4480-179" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4480-179).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.216' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-errors-abstract" />
      <sch:assert id="a-4480-296" test="count(cda:templateId[@root][@extension='2020-09-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4480-296) such that it SHALL contain exactly one [1..1] @root (CONF:4480-298). SHALL contain exactly one [1..1] @extension="2020-09-01" (CONF:4480-299).</sch:assert>
      <sch:assert id="a-4480-297" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4480-297).</sch:assert>
      <sch:assert id="a-4480-300" test="cda:code[@code='29762-2' and @codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @code="29762-2" Social history (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4480-300).</sch:assert>
      <sch:assert id="a-4480-301" test="cda:code[@codeSystem]">This code SHALL contain exactly one [1..1] @codeSystem, which SHALL be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) STATIC (CONF:4480-301).</sch:assert>
      <sch:assert id="a-4480-302" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:4480-302).</sch:assert>
      <sch:assert id="a-4480-303" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:4480-303).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-errors-abstract" />
      <sch:assert id="a-4482-1" test="count(cda:componentOf)=1">SHALL contain exactly one [1..1] componentOf (CONF:4482-1).</sch:assert>
      <sch:assert id="a-4482-2" test="cda:componentOf[count(cda:encompassingEncounter)=1]">This componentOf SHALL contain exactly one [1..1] encompassingEncounter (CONF:4482-2).</sch:assert>
      <sch:assert id="a-4482-5" test="cda:componentOf/cda:encompassingEncounter[count(cda:effectiveTime)=1]">This encompassingEncounter SHALL contain exactly one [1..1] effectiveTime (CONF:4482-5).</sch:assert>
      <sch:assert id="a-4482-20" test="cda:componentOf/cda:encompassingEncounter/cda:effectiveTime[count(cda:low)=1]">This effectiveTime SHALL contain exactly one [1..1] low (CONF:4482-20).</sch:assert>
      <sch:assert id="a-4482-7" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty[count(cda:assignedEntity)=1]">The responsibleParty, if present, SHALL contain exactly one [1..1] assignedEntity (CONF:4482-7).</sch:assert>
      <sch:assert id="a-4482-8" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity[count(cda:id) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] id (CONF:4482-8).</sch:assert>
      <sch:assert id="a-4482-9" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity[count(cda:assignedPerson)=1]">This assignedEntity SHALL contain exactly one [1..1] assignedPerson (CONF:4482-9).</sch:assert>
      <sch:assert id="a-4482-25-c" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson[count(cda:name)=1]">This assignedPerson SHALL contain exactly one [1..1] US Realm Person Name (PN.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.1.1) (CONF:4482-25).</sch:assert>
      <sch:assert id="a-4482-10" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity[count(cda:representedOrganization)=1]">This assignedEntity SHALL contain exactly one [1..1] representedOrganization (CONF:4482-10).</sch:assert>
      <sch:assert id="a-4482-27-c" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization[count(cda:addr)=1]">This representedOrganization SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-27).</sch:assert>
      <sch:assert id="a-4482-125-c" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity[count(cda:addr) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-125).</sch:assert>
      <sch:assert id="a-4482-12" test="not(cda:componentOf/cda:encompassingEncounter/cda:location) or cda:componentOf/cda:encompassingEncounter/cda:location[count(cda:healthCareFacility)=1]">The location, if present, SHALL contain exactly one [1..1] healthCareFacility (CONF:4482-12).</sch:assert>
      <sch:assert id="a-4482-13" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility[count(cda:id) &gt; 0]">This healthCareFacility SHALL contain at least one [1..*] id (CONF:4482-13).</sch:assert>
      <sch:assert id="a-4482-15" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility[count(cda:location)=1]">This healthCareFacility SHALL contain exactly one [1..1] location (CONF:4482-15).</sch:assert>
      <sch:assert id="a-4482-32-c" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location[count(cda:addr)=1]">This location SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-32).</sch:assert>
      <sch:assert id="a-4482-16" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility[count(cda:serviceProviderOrganization)=1]">This healthCareFacility SHALL contain exactly one [1..1] serviceProviderOrganization (CONF:4482-16).</sch:assert>
      <sch:assert id="a-4482-126-c" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization[count(cda:addr)=1]">This serviceProviderOrganization SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-126).</sch:assert>
      <sch:assert id="a-4482-14" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility[count(cda:code)=1]">This healthCareFacility SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ServiceDeliveryLocationRoleType urn:oid:2.16.840.1.113883.1.11.17660 DYNAMIC (CONF:4482-14).</sch:assert>
      <sch:assert id="a-4482-3" test="cda:componentOf/cda:encompassingEncounter[count(cda:id) &gt; 0]">This encompassingEncounter SHALL contain at least one [1..*] id (CONF:4482-3).</sch:assert>
      <sch:assert id="a-4482-35" test="count(cda:component)=1">SHALL contain exactly one [1..1] component (CONF:4482-35).</sch:assert>
      <sch:assert id="a-4482-85" test="cda:component[count(cda:structuredBody)=1]">This component SHALL contain exactly one [1..1] structuredBody (CONF:4482-85).</sch:assert>
      <sch:assert id="a-4482-86" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22.1' and @extension='2015-08-01']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-86) such that it SHALL contain exactly one [1..1] Encounters Section (entries required) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.22.1:2015-08-01) (CONF:4482-90).</sch:assert>
      <sch:assert id="a-4482-87" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2015-08-01']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-87) such that it SHALL contain exactly one [1..1] Social History Section (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.17:2015-08-01) (CONF:4482-91).</sch:assert>
      <sch:assert id="a-4482-88" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.38' and @extension='2014-06-09']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-88) such that it SHALL contain exactly one [1..1] Medications Administered Section (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.38:2014-06-09) (CONF:4482-92).</sch:assert>
      <sch:assert id="a-4482-89" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3.1' and @extension='2015-08-01']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-89) such that it SHALL contain exactly one [1..1] Results Section (entries required) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.3.1:2015-08-01) (CONF:4482-93).</sch:assert>
      <sch:assert id="a-4482-97" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.4']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-97) such that it SHALL contain exactly one [1..1] History of Present Illness Section (identifier: urn:oid:1.3.6.1.4.1.19376.1.5.3.1.3.4) (CONF:4482-100).</sch:assert>
      <sch:assert id="a-4482-98" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-98) such that it SHALL contain exactly one [1..1] Chief Complaint Section (identifier: urn:oid:1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1) (CONF:4482-101).</sch:assert>
      <sch:assert id="a-4482-99" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5.1' and @extension='2015-08-01']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-99) such that it SHALL contain exactly one [1..1] Problem Section (entries required) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.5.1:2015-08-01) (CONF:4482-102).</sch:assert>
      <sch:assert id="a-4482-94" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-94) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2" (CONF:4482-95). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-96).</sch:assert>
      <sch:assert id="a-4482-103" test="count(cda:recordTarget)=1">SHALL contain exactly one [1..1] recordTarget (CONF:4482-103).</sch:assert>
      <sch:assert id="a-4482-104" test="cda:recordTarget[count(cda:patientRole)=1]">This recordTarget SHALL contain exactly one [1..1] patientRole (CONF:4482-104).</sch:assert>
      <sch:assert id="a-4482-105" test="cda:recordTarget/cda:patientRole[count(cda:patient)=1]">This patientRole SHALL contain exactly one [1..1] patient (CONF:4482-105).</sch:assert>
      <sch:assert id="a-4482-115-c" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:addr) &gt; 0]">The guardian, if present, SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-115).</sch:assert>
      <sch:assert id="a-4482-147-c" test="cda:recordTarget/cda:patientRole[count(cda:addr) &gt; 0]">This patientRole SHALL contain at least one [1..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-147).</sch:assert>
      <sch:assert id="a-4482-127" test="count(cda:author) &gt; 0">SHALL contain at least one [1..*] author (CONF:4482-127).</sch:assert>
      <sch:assert id="a-4482-142-c" test="not(existence_schema_tested)">Such authors SHALL contain exactly one [1..1] US Realm Date and Time (DTM.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.4) (CONF:4482-142).</sch:assert>
      <sch:assert id="a-4482-128" test="cda:author[count(cda:assignedAuthor)=1]">Such authors SHALL contain exactly one [1..1] assignedAuthor (CONF:4482-128).</sch:assert>
      <sch:assert id="a-4482-141" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-141).</sch:assert>
      <sch:assert id="a-4482-397" test="not(cda:documentationOf) or cda:documentationOf[count(cda:serviceEvent)=1]">The documentationOf, if present, SHALL contain exactly one [1..1] serviceEvent (CONF:4482-397).</sch:assert>
      <sch:assert id="a-4482-457" test="not(cda:relatedDocument) or cda:relatedDocument[count(cda:parentDocument)=1]">The relatedDocument, if present, SHALL contain exactly one [1..1] parentDocument (CONF:4482-457).</sch:assert>
      <sch:assert id="a-4482-124" test="cda:componentOf/cda:encompassingEncounter/cda:effectiveTime[not(@nullFlavor)]">This effectiveTime SHALL NOT contain [0..0] @nullFlavor (CONF:4482-124).</sch:assert>
      <sch:assert id="a-4482-22" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:id) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:id[@root]">Such ids SHALL contain exactly one [1..1] @root (CONF:4482-22).</sch:assert>
      <sch:assert id="a-4482-26" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization[count(cda:name)=1]">This representedOrganization SHALL contain exactly one [1..1] name (CONF:4482-26).</sch:assert>
      <sch:assert id="a-4482-24" test="not(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity) or cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity[count(cda:telecom) &gt; 0]">This assignedEntity SHALL contain at least one [1..*] telecom (CONF:4482-24).</sch:assert>
      <sch:assert id="a-4482-28" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:id) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:id[@root]">Such ids SHALL contain exactly one [1..1] @root (CONF:4482-28).</sch:assert>
      <sch:assert id="a-4482-33" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization[count(cda:name)=1]">This serviceProviderOrganization SHALL contain exactly one [1..1] name (CONF:4482-33).</sch:assert>
      <sch:assert id="a-4482-34" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization[count(cda:telecom) &gt; 0]">This serviceProviderOrganization SHALL contain at least one [1..*] telecom (CONF:4482-34).</sch:assert>
      <sch:assert id="a-4482-401-c" test="cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:code[not(@nullFlavor)] or cda:componentOf/cda:encompassingEncounter/cda:code[@code='PHC2237']">This code SHALL NOT contain [0..0] @nullFlavor (CONF:4482-401).</sch:assert>
      <sch:assert id="a-4482-4" test="cda:componentOf/cda:encompassingEncounter[count(cda:code)=1]">This encompassingEncounter SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ActEncounterCode urn:oid:2.16.840.1.113883.1.11.13955 DYNAMIC (CONF:4482-4).</sch:assert>
      <sch:assert id="a-4482-594-c" test="(count(cda:componentOf/cda:encompassingEncounter/cda:code[@code='PHC2237'])=0 and count(cda:componentOf/cda:encompassingEncounter/cda:responsibleParty)=1 and count(cda:componentOf/cda:encompassingEncounter/cda:location)=1) or count(cda:componentOf/cda:encompassingEncounter/cda:code[@code='PHC2237'])=1">EncompassingEncounter/responsibleParty and encompassingEncounter/location SHALL be present when not( encompassingEncounter/code="PHC2237" | codeSystem="2.16.840.1.114222.4.5.274" | codeSystemName="PHIN VS (CDC Local Coding System)" (External Encounter)) (CONF:4482-594).</sch:assert>
      <sch:assert id="a-4482-116" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:telecom) &gt; 0]">The guardian, if present, SHALL contain at least one [1..*] telecom (CONF:4482-116).</sch:assert>
      <sch:assert id="a-4482-129" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:guardianPerson)=1]">The guardian, if present, SHALL contain exactly one [1..1] guardianPerson (CONF:4482-129).</sch:assert>
      <sch:assert id="a-4482-403-c" test="( not( cda:recordTarget/cda:patientRole/cda:patient/sdtc:deceasedInd[ @value='true']) and not( cda:recordTarget/cda:patientRole/cda:patient/sdtc:deceasedTime)) or ( cda:recordTarget/cda:patientRole/cda:patient/sdtc:deceasedInd[ @value='true'] and cda:recordTarget/cda:patientRole/cda:patient/sdtc:deceasedTime)">If sdtc:deceasedInd is true then sdtc:deceasedTime *SHALL* be present (CONF:4482-403).</sch:assert>
      <sch:assert id="a-4482-130" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:languageCommunication) &gt; 0]">This patient SHALL contain at least one [1..*] languageCommunication (CONF:4482-130).</sch:assert>
      <sch:assert id="a-4482-404" test="cda:recordTarget/cda:patientRole/cda:patient[count(sdtc:deceasedInd)=1]">This patient SHALL contain exactly one [1..1] sdtc:deceasedInd (CONF:4482-404).</sch:assert>
      <sch:assert id="a-4482-595" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:name) &gt; 0]">This patient SHALL contain at least one [1..*] name (CONF:4482-595).</sch:assert>
      <sch:assert id="a-4482-146" test="cda:recordTarget/cda:patientRole[count(cda:id) &gt; 0]">This patientRole SHALL contain at least one [1..*] id (CONF:4482-146).</sch:assert>
      <sch:assert id="a-4482-144" test="cda:author/cda:time[not(@nullFlavor)]">This time SHALL NOT contain [0..0] @nullFlavor (CONF:4482-144).</sch:assert>
      <sch:assert id="a-4482-599-c" test="cda:author[count(cda:assignedAuthor)=1] and ((cda:componentOf/cda:encompassingEncounter/cda:code[@code='PHC2237'] and cda:author/cda:assignedAuthor/cda:assignedPerson and cda:author/cda:assignedAuthor/cda:representedOrganization) or not(cda:componentOf/cda:encompassingEncounter/cda:code[@code='PHC2237']))">Where a trigger occurs outside of an encounter (encompassingEncounter/code = PHC2237) both author/assignedAuthor/authorPerson and author/assignedAuthor/representedOrganization SHALL be present (CONF:4482-599).</sch:assert>
      <sch:assert id="a-4482-143" test="cda:effectiveTime[not(@nullFlavor)]">This effectiveTime SHALL NOT contain [0..0] @nullFlavor (CONF:4482-143).</sch:assert>
      <sch:assert id="a-4482-398" test="not(cda:documentationOf/cda:serviceEvent) or cda:documentationOf/cda:serviceEvent[count(cda:code)=1]">This serviceEvent SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet eICR Initiation urn:oid:2.16.840.1.113883.10.20.15.2.5.11 DYNAMIC (CONF:4482-398).</sch:assert>
      <sch:assert id="a-4482-107" test="count(cda:code[@code='55751-2'][@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code="55751-2" Public Health Case report (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4482-107).</sch:assert>
      <sch:assert id="a-4482-109" test="count(cda:title[text()='Initial Public Health Case Report'])=1">SHALL contain exactly one [1..1] title="Initial Public Health Case Report" (CONF:4482-109).</sch:assert>
      <sch:assert id="a-4482-460" test="not(cda:relatedDocument/cda:parentDocument) or cda:relatedDocument/cda:parentDocument[count(cda:setId)=1]">This parentDocument SHALL contain exactly one [1..1] setId (CONF:4482-460).</sch:assert>
      <sch:assert id="a-4482-461" test="not(cda:relatedDocument/cda:parentDocument) or cda:relatedDocument/cda:parentDocument[count(cda:versionNumber)=1]">This parentDocument SHALL contain exactly one [1..1] versionNumber (CONF:4482-461).</sch:assert>
      <sch:assert id="a-4482-458" test="count(cda:setId)=1">SHALL contain exactly one [1..1] setId (CONF:4482-458).</sch:assert>
      <sch:assert id="a-4482-459" test="count(cda:versionNumber)=1">SHALL contain exactly one [1..1] versionNumber (CONF:4482-459).</sch:assert>
      <sch:assert id="a-4482-596-c" test="((cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC1464'] or cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC2235']) and //cda:observation/cda:templateId[ @root='2.16.840.1.113883.10.20.15.2.3.5']) or (not(cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC1464']) and not(cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC2235']))">A manually/alternately initiated eICR SHALL contain at least one Initial Case Report Initiation Reason Observation (CONF:4482-596).</sch:assert>
      <sch:assert id="a-4482-597-c" test="not( //cda:observation/cda:templateId[ @root='2.16.840.1.113883.10.20.15.2.3.5']) or ((cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC1464'] or cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC2235']) and //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.5'])">If the "Initial Case Report Initiation Reason" template is present then this SHALL be a manually/alternately initiated eICR and documentationOf/serviceEvent SHALL be present with code PHC1464: "Manually Initiated eICR or code PHC2235: "Alternately Initiated eICR" (CONF:4482-597).</sch:assert>
      <sch:assert id="a-4482-598-c" test="(cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC1464'] or cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC2235']) or ((not(cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC1464']) and not(cda:documentationOf/cda:serviceEvent/cda:code[ @code='PHC2235'])) and (//cda:observation/cda:templateId[ @root='2.16.840.1.113883.10.20.15.2.3.4'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.3'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.2'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.35'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.36'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.38'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.41'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.42'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.43'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.44'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.45'] or //cda:observation/cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.46']))">An automatically initiated eICR SHALL contain at least one of Initial Case Report Trigger Code Lab Test Order, Initial Case Report Trigger Code Problem Observation, Initial Case Report Trigger Code Result Observation, Initial Case Report Trigger Code Medication Information, Initial Case Report Trigger Code Result Organizer, Initial Case Report Trigger Code Immunization Medication Information, Initial Case Report Trigger Code Planned Act, Initial Case Report Trigger Code Planned Observation, Initial Case Report Trigger Code Planned Procedure, Initial Case Report Trigger Code Procedure Activity Act, Initial Case Report Trigger Code Procedure Activity Observation, or Initial Case Report Trigger Code Procedure Activity Procedure (CONF:4482-598).</sch:assert>
      <sch:assert id="a-4482-1041" test="not(cda:participant) or cda:participant[count(cda:associatedEntity)=1]">The participant, if present, SHALL contain exactly one [1..1] associatedEntity (CONF:4482-1041).</sch:assert>
      <sch:assert id="a-4482-1042" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[count(cda:id) &gt; 0]">This associatedEntity SHALL contain at least one [1..*] id (CONF:4482-1042).</sch:assert>
      <sch:assert id="a-4482-1043" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[count(cda:code)=1]">This associatedEntity SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ServiceDeliveryLocationRoleType urn:oid:2.16.840.1.113883.1.11.17660 DYNAMIC (CONF:4482-1043).</sch:assert>
      <sch:assert id="a-4482-1044" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[count(cda:addr[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.2']])=1]">This associatedEntity SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-1044).</sch:assert>
      <sch:assert id="a-4482-1045" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[count(cda:scopingOrganization)=1]">This associatedEntity SHALL contain exactly one [1..1] scopingOrganization (CONF:4482-1045).</sch:assert>
      <sch:assert id="a-4482-1046" test="not(cda:participant/cda:associatedEntity/cda:scopingOrganization) or cda:participant/cda:associatedEntity/cda:scopingOrganization[count(cda:addr[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.2']])=1]">This scopingOrganization SHALL contain exactly one [1..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:4482-1046).</sch:assert>
      <sch:assert id="a-4482-1047" test="not(cda:participant) or cda:participant[@typeCode='LOC']">The participant, if present, SHALL contain exactly one [1..1] @typeCode="LOC" location (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-1047).</sch:assert>
      <sch:assert id="a-4482-1048" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[@classCode='SDLOC']">This associatedEntity SHALL contain exactly one [1..1] @classCode="SDLOC" Service Delivery Location (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110) (CONF:4482-1048).</sch:assert>
      <sch:assert id="a-4482-1049" test="not(cda:participant/cda:associatedEntity/cda:id) or cda:participant/cda:associatedEntity/cda:id[@root]">Such ids SHALL contain exactly one [1..1] @root (CONF:4482-1049).</sch:assert>
      <sch:assert id="a-4482-1051" test="not(cda:participant/cda:associatedEntity/cda:code) or cda:participant/cda:associatedEntity/cda:code[not(@nullFlavor)]">This code SHALL NOT contain [0..0] @nullFlavor (CONF:4482-1051).</sch:assert>
      <sch:assert id="a-4482-1052" test="not(cda:participant/cda:associatedEntity/cda:scopingOrganization) or cda:participant/cda:associatedEntity/cda:scopingOrganization[count(cda:name)=1]">This scopingOrganization SHALL contain exactly one [1..1] name (CONF:4482-1052).</sch:assert>
      <sch:assert id="a-4482-1053" test="not(cda:participant/cda:associatedEntity/cda:scopingOrganization) or cda:participant/cda:associatedEntity/cda:scopingOrganization[count(cda:telecom) &gt; 0]">This scopingOrganization SHALL contain at least one [1..*] telecom (CONF:4482-1053).</sch:assert>
      <sch:assert id="a-4482-1086" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.12']])=1])=1]">This structuredBody SHALL contain exactly one [1..1] component (CONF:4482-1086) such that it SHALL contain exactly one [1..1] Reason for Visit Section (identifier: urn:oid:2.16.840.1.113883.10.20.22.2.12) (CONF:4482-1087).</sch:assert>
      <sch:assert id="a-4482-1132" test="not(cda:author/cda:assignedAuthor/cda:assignedPerson) or cda:author/cda:assignedAuthor/cda:assignedPerson[count(cda:name) &gt; 0]">The assignedPerson, if present, SHALL contain at least one [1..*] name (CONF:4482-1132).</sch:assert>
      <sch:assert id="a-4482-1133" test="not(cda:author/cda:assignedAuthor/cda:representedOrganization) or cda:author/cda:assignedAuthor/cda:representedOrganization[count(cda:addr)=1]">The representedOrganization, if present, SHALL contain exactly one [1..1] addr (CONF:4482-1133).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-326-branch-326-errors-abstract" abstract="true">
      <sch:assert id="a-4482-327-branch-326-c" test="not(tested)">SHALL contain exactly one [1..1] Birth Sex Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.200:2016-06-01) (CONF:4482-327).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-326-branch-326-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:component/cda:structuredBody/cda:component[cda:section]/cda:entry[cda:observation]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-326-branch-326-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-334-branch-334-errors-abstract" abstract="true">
      <sch:assert id="a-4482-335-branch-334-c" test="not(tested)">SHALL contain exactly one [1..1] Travel History (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.1:2021-01-01) (CONF:4482-335).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-334-branch-334-errors" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:component/cda:structuredBody/cda:component[cda:section]/cda:entry[cda:act]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-334-branch-334-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-610" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.40' and @extension='2021-01-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entry (CONF:4482-610) such that it SHALL contain exactly one [1..1] Emergency Outbreak Information Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.40:2021-01-01) (CONF:4482-635).</sch:assert>
      <sch:assert id="a-4482-613" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-613).</sch:assert>
      <sch:assert id="a-4482-618" test="cda:code[@code='83910-0']">This code SHALL contain exactly one [1..1] @code="83910-0" Public health Note (CONF:4482-618).</sch:assert>
      <sch:assert id="a-4482-620" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:4482-620).</sch:assert>
      <sch:assert id="a-4482-621" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:4482-621).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.4' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-errors-abstract" />
      <sch:assert id="a-4482-612" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.4'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-612) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.2.4" (CONF:4482-616). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-617).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-623" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.40'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-623) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.40" (CONF:4482-625). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-626).</sch:assert>
      <sch:assert id="a-4482-624" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-624).</sch:assert>
      <sch:assert id="a-4482-627" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-627).</sch:assert>
      <sch:assert id="a-4482-628" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-628).</sch:assert>
      <sch:assert id="a-4482-633" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-633).</sch:assert>
      <sch:assert id="a-4482-634" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-634).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.40' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-638" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.41'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-638) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.41" (CONF:4482-650). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-651).</sch:assert>
      <sch:assert id="a-4482-642" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-642).</sch:assert>
      <sch:assert id="a-4482-648" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-648).</sch:assert>
      <sch:assert id="a-4482-649" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2014-09-01 (CONF:4482-649).</sch:assert>
      <sch:assert id="a-4482-658" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Reportable Condition Trigger Codes (RCTC) urn:oid:2.16.840.1.114222.4.11.7508 DYNAMIC (CONF:4482-658).</sch:assert>
      <sch:assert id="a-4482-662" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Reportable Condition Trigger Codes (RCTC) urn:oid:2.16.840.1.114222.4.11.7508 DYNAMIC (CONF:4482-662).</sch:assert>
      <sch:assert id="a-4482-665-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-665).</sch:assert>
      <sch:assert id="a-4482-666-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-666).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.41' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-667" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.42'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-667) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.42" (CONF:4482-681). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-682).</sch:assert>
      <sch:assert id="a-4482-671" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-671).</sch:assert>
      <sch:assert id="a-4482-679" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-679).</sch:assert>
      <sch:assert id="a-4482-680" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.23']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Act/Encounter/Procedure) urn:oid:2.16.840.1.113883.11.20.9.23 STATIC 2011-09-30 (CONF:4482-680).</sch:assert>
      <sch:assert id="a-4482-684" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Reportable Condition Trigger Codes (RCTC) urn:oid:2.16.840.1.114222.4.11.7508 DYNAMIC (CONF:4482-684).</sch:assert>
      <sch:assert id="a-4482-687" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-687).</sch:assert>
      <sch:assert id="a-4482-690-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-690).</sch:assert>
      <sch:assert id="a-4482-691-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-691).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.42' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-692" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.22.4.44'][@extension='2014-06-09'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-692) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.22.4.44" (CONF:4482-705). SHALL contain exactly one [1..1] @extension="2014-06-09" (CONF:4482-706).</sch:assert>
      <sch:assert id="a-4482-703" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-703).</sch:assert>
      <sch:assert id="a-4482-704" test="@moodCode and @moodCode=document('CDAR2_IG_PHCASERPT_R2_D3_VOCABULARY.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.25']/voc:code/@value">SHALL contain exactly one [1..1] @moodCode, which SHALL be selected from ValueSet Planned moodCode (Observation) urn:oid:2.16.840.1.113883.11.20.9.25 STATIC 2011-09-30 (CONF:4482-704).</sch:assert>
      <sch:assert id="a-4482-711-c" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:4482-711).</sch:assert>
      <sch:assert id="a-4482-713" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-713).</sch:assert>
      <sch:assert id="a-4482-716" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-716).</sch:assert>
      <sch:assert id="a-4482-719-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-719).</sch:assert>
      <sch:assert id="a-4482-720-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-720).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.43' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-errors-abstract" />
      <sch:assert id="a-4482-157" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.3'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-157) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.3" (CONF:4482-169). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-170).</sch:assert>
      <sch:assert id="a-4482-160-c" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD" (CONF:4482-160).</sch:assert>
      <sch:assert id="a-4482-176" test="cda:value[@xsi:type='CD'][@code]">This value SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-176).</sch:assert>
      <sch:assert id="a-4482-477" test="not(cda:value[@xsi:type='CD']/cda:translation) or cda:value[@xsi:type='CD']/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which MAY be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-477).</sch:assert>
      <sch:assert id="a-4482-535-c" test="(cda:value/cda:translation/@sdtc:valueSet and cda:value/cda:translation/@sdtc:valueSetVersion) or (not(cda:value/cda:translation/@sdtc:valueSet) and not(cda:value/cda:translation/@sdtc:valueSetVersion))">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-535).</sch:assert>
      <sch:assert id="a-4482-536-c" test="(cda:value/@sdtc:valueSet and cda:value/@sdtc:valueSetVersion) or (not(cda:value/@sdtc:valueSet) and not(cda:value/@sdtc:valueSetVersion))">If either value[xsi:type="CD"]/@sdtc:valueSet or value[xsi:type="CD"]/@sdtc:valueSetVersion is present then both value[xsi:type="CD"]/@sdtc:valueSet and value[xsi:type="CD"]/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-536).</sch:assert>
      <sch:assert id="a-4482-183" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-183).</sch:assert>
      <sch:assert id="a-4482-184" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4482-184).</sch:assert>
      <sch:assert id="a-4482-296" test="@negationInd">SHALL contain exactly one [1..1] @negationInd (CONF:4482-296).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.3' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-721" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.45'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-721) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.45" (CONF:4482-744). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-745).</sch:assert>
      <sch:assert id="a-4482-722" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-722).</sch:assert>
      <sch:assert id="a-4482-742" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" Act (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-742).</sch:assert>
      <sch:assert id="a-4482-743" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4482-743).</sch:assert>
      <sch:assert id="a-4482-863" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-863).</sch:assert>
      <sch:assert id="a-4482-867-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-867).</sch:assert>
      <sch:assert id="a-4482-868-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-868).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.45' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-769" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.46'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-769) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.46" (CONF:4482-796). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-797).</sch:assert>
      <sch:assert id="a-4482-770" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-770).</sch:assert>
      <sch:assert id="a-4482-794" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" Observation (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-794).</sch:assert>
      <sch:assert id="a-4482-795" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4482-795).</sch:assert>
      <sch:assert id="a-4482-861" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-861).</sch:assert>
      <sch:assert id="a-4482-869-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-869).</sch:assert>
      <sch:assert id="a-4482-870-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-870).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.46' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-errors-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-errors-abstract" />
      <sch:assert id="a-4482-825" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.44'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-825) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.44" (CONF:4482-856). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-857).</sch:assert>
      <sch:assert id="a-4482-826" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-826).</sch:assert>
      <sch:assert id="a-4482-854" test="@classCode='PROC'">SHALL contain exactly one [1..1] @classCode="PROC" Procedure (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6 STATIC) (CONF:4482-854).</sch:assert>
      <sch:assert id="a-4482-855" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001 STATIC) (CONF:4482-855).</sch:assert>
      <sch:assert id="a-4482-865" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-865).</sch:assert>
      <sch:assert id="a-4482-871-c" test="not(tested)">If either translation/@sdtc:valueSet or translation/@sdtc:valueSetVersion is present then both translation/@sdtc:valueSet and translation/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-871).</sch:assert>
      <sch:assert id="a-4482-872-c" test="not(tested)">If either code/@sdtc:valueSet or code/@sdtc:valueSetVersion is present then both code/@sdtc:valueSet and code/@sdtc:valueSetVersion **SHALL** be present (CONF:4482-872).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-errors" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.44' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-885" test="count(cda:entry[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.34' and @extension='2017-04-01']])=1]) &gt; 0">SHALL contain at least one [1..*] entry (CONF:4482-885) such that it SHALL contain exactly one [1..1] Reportability Response Coded Information Organizer (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.34:2017-04-01) (CONF:4482-896).</sch:assert>
      <sch:assert id="a-4482-888" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-888).</sch:assert>
      <sch:assert id="a-4482-891" test="cda:code[@code='88085-6']">This code SHALL contain exactly one [1..1] @code="88085-6" Reportability response report Document Public health (CONF:4482-891).</sch:assert>
      <sch:assert id="a-4482-893" test="count(cda:title)=1">SHALL contain exactly one [1..1] title (CONF:4482-893).</sch:assert>
      <sch:assert id="a-4482-894" test="count(cda:text)=1">SHALL contain exactly one [1..1] text (CONF:4482-894).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-errors" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.5' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-errors-abstract" />
      <sch:assert id="a-4482-887" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.5'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-887) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.2.5" (CONF:4482-889). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-890).</sch:assert>
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-899" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.47'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-899) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.47" (CONF:4482-900). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-901).</sch:assert>
      <sch:assert id="a-4482-902" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Disability Status urn:oid:2.16.840.1.113762.1.4.1099.49 DYNAMIC (CONF:4482-902).</sch:assert>
      <sch:assert id="a-4482-903" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-903).</sch:assert>
      <sch:assert id="a-4482-904" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-904).</sch:assert>
      <sch:assert id="a-4482-905" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4482-905).</sch:assert>
      <sch:assert id="a-4482-906" test="count(cda:value[@xsi:type='BL'])=1">SHALL contain exactly one [1..1] value with @xsi:type="BL" (CONF:4482-906).</sch:assert>
      <sch:assert id="a-4482-907" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-907).</sch:assert>
      <sch:assert id="a-4482-908" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-908).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.47' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-911" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.48'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-911) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.48" (CONF:4482-912). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-913).</sch:assert>
      <sch:assert id="a-4482-914" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet TribalEntityUS urn:oid:2.16.840.1.113883.1.11.11631 DYNAMIC (CONF:4482-914).</sch:assert>
      <sch:assert id="a-4482-915" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-915).</sch:assert>
      <sch:assert id="a-4482-916" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-916).</sch:assert>
      <sch:assert id="a-4482-917" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4482-917).</sch:assert>
      <sch:assert id="a-4482-918" test="count(cda:value[@xsi:type='BL'])=1">SHALL contain exactly one [1..1] value with @xsi:type="BL" (CONF:4482-918).</sch:assert>
      <sch:assert id="a-4482-919" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-919).</sch:assert>
      <sch:assert id="a-4482-920" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-920).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.48' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-923" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.49'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-923) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.49" (CONF:4482-924). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-925).</sch:assert>
      <sch:assert id="a-4482-926" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-926).</sch:assert>
      <sch:assert id="a-4482-927" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-927).</sch:assert>
      <sch:assert id="a-4482-928" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-928).</sch:assert>
      <sch:assert id="a-4482-929" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4482-929).</sch:assert>
      <sch:assert id="a-4482-930" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:4482-930).</sch:assert>
      <sch:assert id="a-4482-931" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-931).</sch:assert>
      <sch:assert id="a-4482-932" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-932).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.49' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-935" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.50'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-935) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.50" (CONF:4482-946). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-947).</sch:assert>
      <sch:assert id="a-4482-938" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-938).</sch:assert>
      <sch:assert id="a-4482-948" test="@classCode='CLUSTER'">SHALL contain exactly one [1..1] @classCode="CLUSTER" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-948).</sch:assert>
      <sch:assert id="a-4482-949" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" Event (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-949).</sch:assert>
      <sch:assert id="a-4482-950" test="cda:statusCode[@code='completed']">This statusCode SHALL contain exactly one [1..1] @code="completed" (CodeSystem: HL7ActStatus urn:oid:2.16.840.1.113883.5.14) (CONF:4482-950).</sch:assert>
      <sch:assert id="a-4482-951" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Transport vehicle type urn:oid:2.16.840.1.113762.1.4.1099.50 DYNAMIC (CONF:4482-951).</sch:assert>
      <sch:assert id="a-4482-953" test="not(cda:component) or cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.49' and @extension='2021-01-01']])=1]">The component, if present, SHALL contain exactly one [1..1] Transportation Details Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.49:2021-01-01) (CONF:4482-953).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-errors" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.50' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-240" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.1'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-240) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.1" (CONF:4482-244). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-245).</sch:assert>
      <sch:assert id="a-4482-251" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-251).</sch:assert>
      <sch:assert id="a-4482-248" test="@classCode='ACT'">SHALL contain exactly one [1..1] @classCode="ACT" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-248).</sch:assert>
      <sch:assert id="a-4482-249" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-249).</sch:assert>
      <sch:assert id="a-4482-250" test="count(cda:id)=1">SHALL contain exactly one [1..1] id (CONF:4482-250).</sch:assert>
      <sch:assert id="a-4482-253" test="cda:code[@code='420008001']">This code SHALL contain exactly one [1..1] @code="420008001" Travel (CONF:4482-253).</sch:assert>
      <sch:assert id="a-4482-254" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:4482-254).</sch:assert>
      <sch:assert id="a-4482-295" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-295).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.1' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-errors-abstract" />
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-errors-abstract" abstract="true">
      <sch:assert id="a-4482-266-branch-257" test="not(cda:participantRole/cda:addr) or cda:participantRole/cda:addr[count(cda:country)=1]">The addr, if present, SHALL contain exactly one [1..1] country, which SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:4482-266).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-errors" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.1' and @extension='2021-01-01']]/cda:participant[cda:participantRole[@classCode='TERR']][@typeCode='LOC']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-959" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.51'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-959) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.51" (CONF:4482-961). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-962).</sch:assert>
      <sch:assert id="a-4482-960" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-960).</sch:assert>
      <sch:assert id="a-4482-963" test="cda:code[@code='280147009']">This code SHALL contain exactly one [1..1] @code="280147009" Type of activity (CONF:4482-963).</sch:assert>
      <sch:assert id="a-4482-964" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:4482-964).</sch:assert>
      <sch:assert id="a-4482-965" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-965).</sch:assert>
      <sch:assert id="a-4482-966" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-966).</sch:assert>
      <sch:assert id="a-4482-968" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Travel Purpose urn:oid:2.16.840.1.114222.4.11.3108 DYNAMIC (CONF:4482-968).</sch:assert>
      <sch:assert id="a-4482-970" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-970).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.51' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-975" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.52'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-975) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.52" (CONF:4482-977). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-978).</sch:assert>
      <sch:assert id="a-4482-976" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-976).</sch:assert>
      <sch:assert id="a-4482-981" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-981).</sch:assert>
      <sch:assert id="a-4482-982" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-982).</sch:assert>
      <sch:assert id="a-4482-983" test="count(cda:id) &gt; 0">SHALL contain at least one [1..*] id (CONF:4482-983).</sch:assert>
      <sch:assert id="a-4482-985" test="count(cda:effectiveTime)=1">SHALL contain exactly one [1..1] effectiveTime (CONF:4482-985).</sch:assert>
      <sch:assert id="a-4482-986" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-986).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.52' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-987" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.4'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-987) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.4" (CONF:4482-988). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-989).</sch:assert>
      <sch:assert id="a-4482-990" test="@typeCode='LOC'">SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-990).</sch:assert>
      <sch:assert id="a-4482-991" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:4482-991).</sch:assert>
      <sch:assert id="a-4482-993" test="cda:participantRole[@classCode='TERR']">This participantRole SHALL contain exactly one [1..1] @classCode="TERR" Territory (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110) (CONF:4482-993).</sch:assert>
      <sch:assert id="a-4482-1002" test="not(cda:participantRole/cda:addr) or cda:participantRole/cda:addr[count(cda:country)=1]">The addr, if present, SHALL contain exactly one [1..1] country, which SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:4482-1002).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.4' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-1006" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.5'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-1006) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.5" (CONF:4482-1009). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-1010).</sch:assert>
      <sch:assert id="a-4482-1007" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:4482-1007).</sch:assert>
      <sch:assert id="a-4482-1011" test="@typeCode='IND'">SHALL contain exactly one [1..1] @typeCode="IND" (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-1011).</sch:assert>
      <sch:assert id="a-4482-1037" test="not(cda:participantRole/cda:playingEntity) or cda:participantRole/cda:playingEntity[@classCode='ANM']">The playingEntity, if present, SHALL contain exactly one [1..1] @classCode="ANM" Animal (CodeSystem: HL7EntityClass urn:oid:2.16.840.1.113883.5.41) (CONF:4482-1037).</sch:assert>
      <sch:assert id="a-4482-1038" test="not(cda:participantRole/cda:playingEntity) or cda:participantRole/cda:playingEntity[count(cda:code)=1]">The playingEntity, if present, SHALL contain exactly one [1..1] code, which SHALL be selected from ValueSet Animal urn:oid:2.16.840.1.114222.4.11.1074 DYNAMIC (CONF:4482-1038).</sch:assert>
      <sch:assert id="a-4482-1134" test="count(sdtc:functionCode)=1">SHALL contain exactly one [1..1] sdtc:functionCode, which SHALL be selected from ValueSet ActClassExposure urn:oid:2.16.840.1.113883.1.11.19832.1 DYNAMIC (CONF:4482-1134).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.5' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-1025" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.6'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-1025) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.4.6" (CONF:4482-1027). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-1028).</sch:assert>
      <sch:assert id="a-4482-1026" test="count(cda:participantRole)=1">SHALL contain exactly one [1..1] participantRole (CONF:4482-1026).</sch:assert>
      <sch:assert id="a-4482-1030" test="@typeCode='IND'">SHALL contain exactly one [1..1] @typeCode="IND" (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-1030).</sch:assert>
      <sch:assert id="a-4482-1031" test="cda:participantRole[count(cda:playingEntity)=1]">This participantRole SHALL contain exactly one [1..1] playingEntity (CONF:4482-1031).</sch:assert>
      <sch:assert id="a-4482-1032" test="cda:participantRole/cda:playingEntity[count(cda:name)=1]">This playingEntity SHALL contain exactly one [1..1] name (CONF:4482-1032).</sch:assert>
      <sch:assert id="a-4482-1035" test="cda:participantRole/cda:playingEntity[@classCode='PSN']">This playingEntity SHALL contain exactly one [1..1] @classCode="PSN" Person (CodeSystem: HL7EntityClass urn:oid:2.16.840.1.113883.5.41) (CONF:4482-1035).</sch:assert>
      <sch:assert id="a-4482-1136" test="count(sdtc:functionCode)=1">SHALL contain exactly one [1..1] sdtc:functionCode (ValueSet: ActClassExposure urn:oid:2.16.840.1.113883.1.11.19832.1 DYNAMIC) (CONF:4482-1136).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-errors" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.6' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-1064" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.53'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-1064) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.53" (CONF:4482-1066). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-1067).</sch:assert>
      <sch:assert id="a-4482-1065" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-1065).</sch:assert>
      <sch:assert id="a-4482-1068" test="cda:code[@code='77983-5']">This code SHALL contain exactly one [1..1] @code="77983-5" Country of usual residence (CONF:4482-1068).</sch:assert>
      <sch:assert id="a-4482-1069" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4482-1069).</sch:assert>
      <sch:assert id="a-4482-1070" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-1070).</sch:assert>
      <sch:assert id="a-4482-1071" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-1071).</sch:assert>
      <sch:assert id="a-4482-1073" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:4482-1073).</sch:assert>
      <sch:assert id="a-4482-1074" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-1074).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.53' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-1075" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.54'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-1075) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.54" (CONF:4482-1077). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-1078).</sch:assert>
      <sch:assert id="a-4482-1076" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-1076).</sch:assert>
      <sch:assert id="a-4482-1079" test="cda:code[@code='186034007']">This code SHALL contain exactly one [1..1] @code="186034007" Ethnicity / related nationality data (observable entity) (CONF:4482-1079).</sch:assert>
      <sch:assert id="a-4482-1080" test="cda:code[@codeSystem='2.16.840.1.113883.6.96']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.96" (CodeSystem: SNOMED CT urn:oid:2.16.840.1.113883.6.96) (CONF:4482-1080).</sch:assert>
      <sch:assert id="a-4482-1081" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-1081).</sch:assert>
      <sch:assert id="a-4482-1082" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-1082).</sch:assert>
      <sch:assert id="a-4482-1084" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:4482-1084).</sch:assert>
      <sch:assert id="a-4482-1085" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-1085).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.54' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-errors">
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-errors-abstract" abstract="true">
      <sch:assert id="a-4482-1106" test="count(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.55'][@extension='2021-01-01'])=1">SHALL contain exactly one [1..1] templateId (CONF:4482-1106) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.10.20.15.2.3.55" (CONF:4482-1108). SHALL contain exactly one [1..1] @extension="2021-01-01" (CONF:4482-1109).</sch:assert>
      <sch:assert id="a-4482-1107" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-1107).</sch:assert>
      <sch:assert id="a-4482-1110" test="cda:code[@code='11370-4']">This code SHALL contain exactly one [1..1] @code="11370-4" Immunization status - Reported (CONF:4482-1110).</sch:assert>
      <sch:assert id="a-4482-1111" test="cda:code[@codeSystem='2.16.840.1.113883.6.1']">This code SHALL contain exactly one [1..1] @codeSystem="2.16.840.1.113883.6.1" (CodeSystem: LOINC urn:oid:2.16.840.1.113883.6.1) (CONF:4482-1111).</sch:assert>
      <sch:assert id="a-4482-1112" test="@classCode='OBS'">SHALL contain exactly one [1..1] @classCode="OBS" (CodeSystem: HL7ActClass urn:oid:2.16.840.1.113883.5.6) (CONF:4482-1112).</sch:assert>
      <sch:assert id="a-4482-1113" test="@moodCode='EVN'">SHALL contain exactly one [1..1] @moodCode="EVN" (CodeSystem: HL7ActMood urn:oid:2.16.840.1.113883.5.1001) (CONF:4482-1113).</sch:assert>
      <sch:assert id="a-4482-1115" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHALL be selected from ValueSet Yes No Unknown (YNU) urn:oid:2.16.840.1.114222.4.11.888 DYNAMIC (CONF:4482-1115).</sch:assert>
      <sch:assert id="a-4482-1116" test="count(cda:statusCode)=1">SHALL contain exactly one [1..1] statusCode (CONF:4482-1116).</sch:assert>
    </sch:rule>
    <sch:rule role="error" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-errors" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.55' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-errors-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.15.3.1-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.3.1']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.1-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings-abstract" abstract="true">
      <sch:assert id="a-81-2018" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:81-2018).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.3.8']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.1-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14' and @extension='2015-08-01']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.2' and @extension='2017-08-01']]/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.5.1.1.3' and @extension='2019-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.5.1.1.3' and @extension='2021-10-14']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.1-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.2-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-warnings-abstract" abstract="true">
      <sch:assert id="a-81-7290" test="@use">SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet PostalAddressUse urn:oid:2.16.840.1.113883.1.11.10637 STATIC 2005-05-01 (CONF:81-7290).</sch:assert>
      <sch:assert id="a-81-7293" test="count(cda:state)=1">SHOULD contain zero or one [0..1] state (ValueSet: StateValueSet urn:oid:2.16.840.1.113883.3.88.12.80.1 DYNAMIC) (CONF:81-7293).</sch:assert>
      <sch:assert id="a-81-7294-c" test="not(tested_here)">SHOULD contain zero or one [0..1] postalCode, which SHOULD be selected from ValueSet PostalCode urn:oid:2.16.840.1.113883.3.88.12.80.2 DYNAMIC (CONF:81-7294).</sch:assert>
      <sch:assert id="a-81-7295" test="count(cda:country)=1">SHOULD contain zero or one [0..1] country, which SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:81-7295).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2014-06-09']]/cda:participant/cda:participantRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.18' and @extension='2014-06-09']]/cda:performer/cda:assignedEntity/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2014-06-09']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.2.3.4']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.15' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:addr | cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.13.20' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.3.5416.1.8981.1.1']]/cda:recordTarget/cda:patientRole/cda:addr | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2015-08-01']]/cda:performer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:performer[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.1' and @extension='2017-08-01']]/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.37.4.2' and @extension='2017-08-01']]/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2017-05-01']]/cda:participant/cda:participantRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='https://mytest.Header123']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:receivedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='https://icHeader.abc-orig']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.3.5416.1.8981.1.1']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:assignedAuthor/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:dataEnterer/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informant/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.7' and @extension='2020-05-19']]/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:participant/cda:associatedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:participant/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:representedOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:location/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility/cda:serviceProviderOrganization/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:recordTarget/cda:patientRole/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:participant/cda:associatedEntity/cda:addr | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:participant/cda:associatedEntity/cda:scopingOrganization/cda:addr | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.61' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:addr | cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.5.7' and @extension='2021-11-09']]/cda:associatedEntity/cda:scopingOrganization/cda:addr">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.2-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.24-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-warnings" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.24']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.24-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.31-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.31']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.31-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.32-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-warnings-abstract" abstract="true">
      <sch:assert id="a-81-7760" test="count(cda:addr) &gt; 0">SHOULD contain zero or more [0..*] addr (CONF:81-7760).</sch:assert>
      <sch:assert id="a-81-7761" test="count(cda:telecom) &gt; 0">SHOULD contain zero or more [0..*] telecom (CONF:81-7761).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-warnings" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.32']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.32-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.37-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-warnings-abstract" abstract="true">
      <sch:assert id="a-81-16837" test="cda:playingDevice[count(cda:code)=1]">This playingDevice SHOULD contain zero or one [0..1] code (CONF:81-16837).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-warnings" context="cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.37']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.37-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-warnings">
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-warnings" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.18']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.18-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-warnings">
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-warnings" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.1.13.2.1-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.2.12-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.12']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.2.12-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-warnings">
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-warnings" context="cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.4']]">
      <sch:extends rule="r-urn-oid-1.3.6.1.4.1.19376.1.5.3.1.3.4-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.53-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.53']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.53-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2014-06-09']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:encounterParticipant[cda:templateId[@root='2.16.840.1.113883.10.20.6.2.2' and @extension='2014-06-09']]/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.5' and @extension='2014-06-09']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.26' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.25' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2014-08-08']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.13.15' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.13.20' and @extension='2014-08-08']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-11-19']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-11-19']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-12-01']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2014-12-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2015-08-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2015-08-01']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.14' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.5' and @extension='2015-08-01']]/cda:participant/cda:associatedEntity/cda:associatedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-01-22']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-02-08']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-10-05']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-10-05']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.202' and @extension='2016-11-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-11-29']]/cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.15' and @extension='2016-11-29']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.48' and @extension='2017-05-01']]/cda:participant/cda:participantRole/cda:playingEntity/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:guardianPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:assignedAuthor/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:dataEnterer/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:informationRecipient/cda:intendedRecipient/cda:informationRecipient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:recordTarget/cda:patientRole/cda:patient/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.13.1' and @extension='2015-01-29']]/cda:componentOf/cda:encompassingEncounter/cda:encounterParticipant/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:componentOf/cda:encompassingEncounter/cda:responsibleParty/cda:assignedEntity/cda:assignedPerson/cda:name | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.9.275.1' and @extension='2021-12-01']]/cda:legalAuthenticator/cda:assignedEntity/cda:assignedPerson/cda:name | cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.500.1' and @extension='2022-06-01']]/cda:performer/cda:assignedEntity/cda:assignedPerson/cda:name">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.1.1-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.64-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-warnings-abstract" abstract="true">
      <sch:assert id="a-81-9433" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']])=1">SHOULD contain zero or one [0..1] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:81-9433).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.64']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.64-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.5.4-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-warnings-abstract" abstract="true">
      <sch:assert id="a-81-10128-c" test="string-length(@value)&gt;=12">**SHOULD** be precise to the minute (CONF:81-10128).</sch:assert>
      <sch:assert id="a-81-10130-c" test="string-length(@value)&lt;10 or ( string-length(@value)&gt;=10 and (contains(@value,'+') or contains(@value,'-')))">If more precise than day, **SHOULD** include time-zone offset (CONF:81-10130).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2014-06-09']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2015-07-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='1.3.6.1.4.1.19376.1.7.3.1.1.19.1.1' and @extension='2014-11-03']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-11-02']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='20160422']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2016-03-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.9999' and @extension='2016-03-23']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2016-12-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.1.2' and @extension='2017-04-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2017-07-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2018-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2019-04-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2019-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:legalAuthenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:authenticator/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1.999999' and @extension='Example']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.24.1.3' and @extension='2020-02-01']]/cda:effectiveTime | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:author/cda:time | cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2020-12-10']]/cda:author/cda:time">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.5.4-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.72-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.72']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.72-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.109-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-28823-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Residence and Accommodation Type urn:oid:2.16.840.1.113883.11.20.9.49 DYNAMIC (CONF:1098-28823).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.109']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.109-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.111-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.111']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.111-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7513-c" test="count(cda:effectiveTime) = 2 and cda:effectiveTime[@operator='A'][@xsi:type='PIVL_TS' or @xsi:type='EIVL_TS']">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-7513) such that it SHALL contain exactly one [1..1] @operator="A" (CONF:1098-9106).</sch:assert>
      <sch:assert id="a-1098-7514" test="count(cda:routeCode)=1">SHOULD contain zero or one [0..1] routeCode, which SHALL be selected from ValueSet SPL Drug Route of Administration Terminology urn:oid:2.16.840.1.113883.3.88.12.3221.8.7 DYNAMIC (CONF:1098-7514).</sch:assert>
      <sch:assert id="a-1098-7526" test="cda:doseQuantity[@unit]">This doseQuantity SHOULD contain zero or one [0..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-7526).</sch:assert>
      <sch:assert id="a-1098-30800-c" test="count(cda:doseQuantity)=1 or count(cda:rateQuantity)=1">Medication Activity **SHOULD** include doseQuantity **OR** rateQuantity (CONF:1098-30800).</sch:assert>
      <sch:assert id="a-1098-31150" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31150).</sch:assert>
      <sch:assert id="a-1098-32950" test="not(cda:routeCode) or cda:routeCode[count(cda:translation) &gt; 0]">The routeCode, if present, SHOULD contain zero or more [0..*] translation, which SHALL be selected from ValueSet Medication Route urn:oid:2.16.840.1.113762.1.4.1099.12 DYNAMIC (CONF:1098-32950).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-32775-branch-7508" test="@value">SHOULD contain zero or one [0..1] @value (CONF:1098-32775).</sch:assert>
      <sch:assert id="a-1098-32776-branch-7508" test="count(cda:low)=1">SHOULD contain zero or one [0..1] low (CONF:1098-32776).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']]/cda:effectiveTime">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-7508-branch-7508-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings">
    <!--Pattern is used in an implied relationship.-->
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7795" test="count(cda:entry[count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.16' and @extension='2014-06-09']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1098-7795) such that it SHALL contain exactly one [1..1] Medication Activity (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.16:2014-06-09) (CONF:1098-10076).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1-2014-06-09-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.1.1-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.10' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.10-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.123-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.123']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.123-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.113-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.113']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.113-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7332" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-7332).</sch:assert>
      <sch:assert id="a-1098-7333" test="not(cda:effectiveTime) or cda:effectiveTime[count(cda:low)=1]">The effectiveTime, if present, SHOULD contain zero or one [0..1] low (CONF:1098-7333).</sch:assert>
      <sch:assert id="a-1098-7334" test="not(cda:effectiveTime) or cda:effectiveTime[count(cda:high)=1]">The effectiveTime, if present, SHOULD contain zero or one [0..1] high (CONF:1098-7334).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.9' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.9-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-19186" test="cda:code[count(cda:originalText)=1]">This code SHOULD contain zero or one [0..1] originalText (CONF:1098-19186).</sch:assert>
      <sch:assert id="a-1098-19190-c" test="count(cda:code[@codeSystem])=0 or cda:code[@codeSystem='2.16.840.1.113883.6.1'] or cda:code[@codeSystem='2.16.840.1.113883.6.96'] or cda:code[@codeSystem='2.16.840.1.113883.6.12'] or cda:code[@codeSystem='2.16.840.1.113883.6.4'] or cda:code[@codeSystem='2.16.840.1.113883.6.13']">This @code **SHOULD** be selected from LOINC (CodeSystem: 2.16.840.1.113883.6.1) or SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96), and **MAY** be selected from CPT-4 (CodeSystem: 2.16.840.1.113883.6.12) or ICD10 PCS (CodeSystem: 2.16.840.1.113883.6.4) or CDT-2 (Code System: 2.16.840.1.113883.6.13) (CONF:1098-19190).</sch:assert>
      <sch:assert id="a-1098-8301" test="count(cda:performer) &gt; 0">SHOULD contain zero or more [0..*] performer (CONF:1098-8301).</sch:assert>
      <sch:assert id="a-1098-8306" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:representedOrganization)=1]">This assignedEntity SHOULD contain zero or one [0..1] representedOrganization (CONF:1098-8306).</sch:assert>
      <sch:assert id="a-1098-8307" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:id) &gt; 0]">The representedOrganization, if present, SHOULD contain zero or more [0..*] id (CONF:1098-8307).</sch:assert>
      <sch:assert id="a-1098-32477" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32477).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.12' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-19203" test="cda:code[count(cda:originalText)=1]">This code SHOULD contain zero or one [0..1] originalText (CONF:1098-19203).</sch:assert>
      <sch:assert id="a-1098-19204" test="not(cda:code/cda:originalText) or cda:code/cda:originalText[count(cda:reference)=1]">The originalText, if present, SHOULD contain zero or one [0..1] reference (CONF:1098-19204).</sch:assert>
      <sch:assert id="a-1098-19205" test="not(cda:code/cda:originalText/cda:reference) or cda:code/cda:originalText/cda:reference[@value]">The reference, if present, SHOULD contain zero or one [0..1] @value (CONF:1098-19205).</sch:assert>
      <sch:assert id="a-1098-19207-c" test="count(cda:code[@codeSystem])=0 or cda:code[@codeSystem='2.16.840.1.113883.6.1'] or cda:code[@codeSystem='2.16.840.1.113883.6.96'] or cda:code[@codeSystem='2.16.840.1.113883.6.12'] or cda:code[@codeSystem='2.16.840.1.113883.6.104'] or cda:code[@codeSystem='2.16.840.1.113883.6.4'] or cda:code[@codeSystem='2.16.840.1.113883.6.13']">This @code **SHOULD** be selected from LOINC (CodeSystem: 2.16.840.1.113883.6.1) or SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96), and **MAY** be selected from CPT-4 (CodeSystem: 2.16.840.1.113883.6.12) or ICD10 PCS (CodeSystem: 2.16.840.1.113883.6.4) or CDT-2 (Code System: 2.16.840.1.113883.6.13) (CONF:1098-19207).</sch:assert>
      <sch:assert id="a-1098-7662" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-7662).</sch:assert>
      <sch:assert id="a-1098-7683" test="count(cda:targetSiteCode) &gt; 0">SHOULD contain zero or more [0..*] targetSiteCode, which SHALL be selected from ValueSet Body Site Value Set urn:oid:2.16.840.1.113883.3.88.12.3221.8.9 DYNAMIC (CONF:1098-7683).</sch:assert>
      <sch:assert id="a-1098-7716" test="not(cda:specimen/cda:specimenRole) or cda:specimen/cda:specimenRole[count(cda:id) &gt; 0]">This specimenRole SHOULD contain zero or more [0..*] id (CONF:1098-7716).</sch:assert>
      <sch:assert id="a-1098-7718" test="count(cda:performer[count(cda:assignedEntity[count(cda:id) &gt; 0][count(cda:addr) &gt; 0][count(cda:telecom) &gt; 0])=1]) &gt; 0">SHOULD contain zero or more [0..*] performer (CONF:1098-7718) such that it SHALL contain exactly one [1..1] assignedEntity (CONF:1098-7720). This assignedEntity SHALL contain at least one [1..*] id (CONF:1098-7722). This assignedEntity SHALL contain at least one [1..*] addr (CONF:1098-7731). This assignedEntity SHALL contain at least one [1..*] telecom (CONF:1098-7732).</sch:assert>
      <sch:assert id="a-1098-32479" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32479).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7733-branch-7718" test="not(cda:assignedEntity) or cda:assignedEntity[count(cda:representedOrganization)=1]">This assignedEntity SHOULD contain zero or one [0..1] representedOrganization (CONF:1098-7733).</sch:assert>
      <sch:assert id="a-1098-7734-branch-7718" test="not(cda:assignedEntity/cda:representedOrganization) or cda:assignedEntity/cda:representedOrganization[count(cda:id) &gt; 0]">The representedOrganization, if present, SHOULD contain zero or more [0..*] id (CONF:1098-7734).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.14' and @extension='2014-06-09']]/cda:performer[cda:assignedEntity[cda:id][cda:addr][cda:telecom]]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-7718-branch-7718-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-19198" test="cda:code[count(cda:originalText)=1]">This code SHOULD contain zero or one [0..1] originalText (CONF:1098-19198).</sch:assert>
      <sch:assert id="a-1098-19199" test="not(cda:code/cda:originalText) or cda:code/cda:originalText[count(cda:reference)=1]">The originalText, if present, SHOULD contain zero or one [0..1] reference (CONF:1098-19199).</sch:assert>
      <sch:assert id="a-1098-19200" test="not(cda:code/cda:originalText/cda:reference) or cda:code/cda:originalText/cda:reference[@value]">The reference, if present, SHOULD contain zero or one [0..1] @value (CONF:1098-19200).</sch:assert>
      <sch:assert id="a-1098-19202-c" test="count(cda:code[@codeSystem])=0 or cda:code[@codeSystem='2.16.840.1.113883.6.1'] or cda:code[@codeSystem='2.16.840.1.113883.6.96'] or cda:code[@codeSystem='2.16.840.1.113883.6.12'] or cda:code[@codeSystem='2.16.840.1.113883.6.4'] or cda:code[@codeSystem='2.16.840.1.113883.6.13']">This @code **SHOULD** be selected from LOINC (CodeSystem: 2.16.840.1.113883.6.1) or SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96), and **MAY** be selected from CPT-4 (CodeSystem: 2.16.840.1.113883.6.12) or ICD10 PCS (CodeSystem: 2.16.840.1.113883.6.4) or CDT-2 (Code System: 2.16.840.1.113883.6.13) (CONF:1098-19202).</sch:assert>
      <sch:assert id="a-1098-8246" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-8246).</sch:assert>
      <sch:assert id="a-1098-8250" test="count(cda:targetSiteCode) &gt; 0">SHOULD contain zero or more [0..*] targetSiteCode, which SHALL be selected from ValueSet Body Site Value Set urn:oid:2.16.840.1.113883.3.88.12.3221.8.9 DYNAMIC (CONF:1098-8250).</sch:assert>
      <sch:assert id="a-1098-8251" test="count(cda:performer) &gt; 0">SHOULD contain zero or more [0..*] performer (CONF:1098-8251).</sch:assert>
      <sch:assert id="a-1098-8256" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:representedOrganization)=1]">This assignedEntity SHOULD contain zero or one [0..1] representedOrganization (CONF:1098-8256).</sch:assert>
      <sch:assert id="a-1098-8257" test="not(cda:performer/cda:assignedEntity/cda:representedOrganization) or cda:performer/cda:assignedEntity/cda:representedOrganization[count(cda:id) &gt; 0]">The representedOrganization, if present, SHOULD contain zero or more [0..*] id (CONF:1098-8257).</sch:assert>
      <sch:assert id="a-1098-32478" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32478).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.13' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.121-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30785" test="count(cda:entryRelationship[@typeCode='REFR'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.143']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:1098-30785) such that it SHALL contain exactly one [1..1] @typeCode="REFR" Refers to (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:1098-30786). SHALL contain exactly one [1..1] Priority Preference (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.143) (CONF:1098-30787).</sch:assert>
      <sch:assert id="a-1098-30995" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-30995).</sch:assert>
      <sch:assert id="a-1098-32335" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-32335).</sch:assert>
      <sch:assert id="a-1098-30784-v" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1098-30784).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.121']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.121-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.36' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.36-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.38' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.38-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7-2014-06-09-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7.1' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.7.1-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.130-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-31699" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-31699).</sch:assert>
      <sch:assert id="a-1098-30342-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Nutrition Recommendations urn:oid:2.16.840.1.113883.1.11.20.2.9 DYNAMIC (CONF:1098-30342).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.130']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.130-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30433" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-30433).</sch:assert>
      <sch:assert id="a-1098-32020" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']])=1">SHOULD contain zero or one [0..1] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32020).</sch:assert>
      <sch:assert id="a-1098-32030-c" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @codeSystem='2.16.840.1.113883.6.96'])=1">This code in a Planned Act **SHOULD** be selected from LOINC (CodeSystem: 2.16.840.1.113883.6.1) *OR* SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96) (CONF:1098-32030).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.39' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30440" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-30440).</sch:assert>
      <sch:assert id="a-1098-31032" test="count(cda:code)=1">SHOULD contain zero or one [0..1] code, which SHOULD be selected from ValueSet Encounter Planned urn:oid:2.16.840.1.113883.11.20.9.52 DYNAMIC (CONF:1098-31032).</sch:assert>
      <sch:assert id="a-1098-32045" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32045).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-warnings" context="cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.40' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.40-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30447" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-30447).</sch:assert>
      <sch:assert id="a-1098-31977-c" test="count(cda:code[@codeSystem])=0 or cda:code[@codeSystem='2.16.840.1.113883.6.1'] or cda:code[@codeSystem='2.16.840.1.113883.6.96'] or cda:code[@codeSystem='2.16.840.1.113883.6.12'] or cda:code[@codeSystem='2.16.840.1.113883.6.4']">The procedure/code in a planned procedure **SHOULD** be selected from LOINC (codeSystem 2.16.840.1.113883.6.1) *OR* SNOMED CT (CodeSystem: 2.16.840.1.113883.6.96), and **MAY** be selected from CPT-4 (CodeSystem: 2.16.840.1.113883.6.12) **OR** ICD10 PCS (CodeSystem: 2.16.840.1.113883.6.4) (CONF:1098-31977).</sch:assert>
      <sch:assert id="a-1098-31979" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']])=1">SHOULD contain zero or one [0..1] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31979).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.41' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30454" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-30454).</sch:assert>
      <sch:assert id="a-1098-32033" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32033).</sch:assert>
      <sch:assert id="a-1098-32044" test="count(cda:targetSiteCode) &gt; 0">SHOULD contain zero or more [0..*] targetSiteCode, which SHALL be selected from ValueSet Body Site Value Set urn:oid:2.16.840.1.113883.3.88.12.3221.8.9 DYNAMIC (CONF:1098-32044).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.44' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30459" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-30459).</sch:assert>
      <sch:assert id="a-1098-31129" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']])=1">SHOULD contain zero or one [0..1] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31129).</sch:assert>
      <sch:assert id="a-1098-32325" test="count(cda:product)=1">SHOULD contain zero or one [0..1] product (CONF:1098-32325).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-warnings" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.43' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.43-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-32046" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']])=1">SHOULD contain zero or one [0..1] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-32046).</sch:assert>
      <sch:assert id="a-1098-32133" test="not(cda:doseQuantity) or cda:doseQuantity[@unit]">The doseQuantity, if present, SHOULD contain zero or one [0..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-32133).</sch:assert>
      <sch:assert id="a-1098-32134" test="not(cda:rateQuantity) or cda:rateQuantity[@unit]">The rateQuantity, if present, SHOULD contain zero or one [0..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-32134).</sch:assert>
      <sch:assert id="a-1098-32943" test="count(cda:effectiveTime[@operator='A'])=1">SHOULD contain exactly one [1..1] effectiveTime (CONF:1098-32943) such that it SHALL contain exactly one [1..1] @operator="A" (CONF:1098-32945).</sch:assert>
      <sch:assert id="a-1098-32952" test="not(cda:routeCode) or cda:routeCode[count(cda:translation) &gt; 0]">The routeCode, if present, SHOULD contain zero or more [0..*] translation, which SHALL be selected from ValueSet Medication Route urn:oid:2.16.840.1.113762.1.4.1099.12 DYNAMIC (CONF:1098-32952).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.42' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-32944-branch-30468" test="@value">SHOULD contain zero or one [0..1] @value (CONF:1098-32944).</sch:assert>
      <sch:assert id="a-1098-32948-branch-30468" test="count(cda:low)=1">SHOULD contain zero or one [0..1] low (CONF:1098-32948).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.42' and @extension='2014-06-09']]/cda:effectiveTime[@xsi:type='IVL_TS']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.42-2014-06-09-30468-branch-30468-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.5' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.5-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7488" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-7488).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.19' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.19-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.141-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.141']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-31676-branch-31673" test="cda:participantRole[count(cda:code)=1]">This participantRole SHOULD contain zero or one [0..1] code, which SHOULD be selected from ValueSet Healthcare Provider Taxonomy urn:oid:2.16.840.1.114222.4.11.1066 DYNAMIC (CONF:1098-31676).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.141']]/cda:participant[@typeCode='IRCP']">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.141-31673-branch-31673-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-31148" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31148).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.78' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.78-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7310" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-7310).</sch:assert>
      <sch:assert id="a-1098-32934" test="cda:code[@code]">This code SHOULD contain zero or one [0..1] @code, which SHOULD be selected from ValueSet Vital Sign Result Type urn:oid:2.16.840.1.113883.3.88.12.80.62 DYNAMIC (CONF:1098-32934).</sch:assert>
      <sch:assert id="a-1098-7301-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:1098-7301).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.27' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.27-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.8' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.8-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-15143" test="count(cda:effectiveTime[count(cda:high)=1])=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-15143) such that it SHALL contain exactly one [1..1] high (CONF:1098-15144).</sch:assert>
      <sch:assert id="a-1098-7434" test="count(cda:repeatNumber)=1">SHOULD contain zero or one [0..1] repeatNumber (CONF:1098-7434).</sch:assert>
      <sch:assert id="a-1098-7436" test="count(cda:quantity)=1">SHOULD contain zero or one [0..1] quantity (CONF:1098-7436).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-warnings" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.17' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.17-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.23' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-7456" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-7456).</sch:assert>
      <sch:assert id="a-1098-7457" test="count(cda:repeatNumber)=1">SHOULD contain zero or one [0..1] repeatNumber (CONF:1098-7457).</sch:assert>
      <sch:assert id="a-1098-7458" test="count(cda:quantity)=1">SHOULD contain zero or one [0..1] quantity (CONF:1098-7458).</sch:assert>
      <sch:assert id="a-1098-7468-c" test="not(cda:performer/cda:assignedEntity) or cda:performer/cda:assignedEntity[count(cda:addr) &gt; 0]">This assignedEntity SHOULD contain zero or one [0..1] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1098-7468).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-warnings" context="cda:supply[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.18' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.18-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.143-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-30958" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-30958).</sch:assert>
      <sch:assert id="a-1098-32327" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:1098-32327).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.143']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.143-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-31152" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1098-31152).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.85' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.85-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-16884-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Patient Education urn:oid:2.16.840.1.113883.11.20.9.34 DYNAMIC (CONF:1098-16884).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.20' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.20-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.119-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-31671" test="cda:assignedAuthor[count(cda:code)=1]">This assignedAuthor SHOULD contain zero or one [0..1] code, which SHOULD be selected from ValueSet Healthcare Provider Taxonomy urn:oid:2.16.840.1.114222.4.11.1066 DYNAMIC (CONF:1098-31671).</sch:assert>
      <sch:assert id="a-1098-32315-c" test="not(tested)">If the content is patient authored the code **SHOULD** be selected from Personal And Legal Relationship Role Type (2.16.840.1.113883.11.20.12.1) (CONF:1098-32315).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-warnings" context="cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.119-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.122-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.122']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.122-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.118-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.118']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.118-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-9014" test="cda:manufacturedMaterial[count(cda:lotNumberText)=1]">This manufacturedMaterial SHOULD contain zero or one [0..1] lotNumberText (CONF:1098-9014).</sch:assert>
      <sch:assert id="a-1098-9012" test="count(cda:manufacturerOrganization)=1">SHOULD contain zero or one [0..1] manufacturerOrganization (CONF:1098-9012).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.54' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-warnings" context="cda:criterion[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.25' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.25-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-32752" test="count(cda:setId)=1">SHOULD contain zero or one [0..1] setId (CONF:1098-32752).</sch:assert>
      <sch:assert id="a-1098-32753" test="count(cda:versionNumber)=1">SHOULD contain zero or one [0..1] versionNumber (CONF:1098-32753).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-warnings" context="cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.115' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.115-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.129-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.129']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.129-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.120-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-warnings-abstract" abstract="true">
      <sch:assert id="a-1098-32130" test="not(cda:doseQuantity) or cda:doseQuantity[@unit]">The doseQuantity, if present, SHOULD contain zero or one [0..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1098-32130).</sch:assert>
      <sch:assert id="a-1098-32951" test="not(cda:routeCode) or cda:routeCode[count(cda:translation) &gt; 0]">The routeCode, if present, SHOULD contain zero or more [0..*] translation, which SHALL be selected from ValueSet Medication Route urn:oid:2.16.840.1.113762.1.4.1099.12 DYNAMIC (CONF:1098-32951).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.120']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.120-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.30.3.34' and @extension='2014-06-09']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.30.3.34-2014-06-09-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-31153" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31153).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.26-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-9934" test="count(cda:entry)=1">SHOULD contain zero or one [0..1] entry (CONF:1198-9934).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.43' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.43-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-8841" test="count(cda:doseQuantity)=1">SHOULD contain zero or one [0..1] doseQuantity (CONF:1198-8841).</sch:assert>
      <sch:assert id="a-1198-31510" test="count(cda:entryRelationship[@typeCode='COMP'][@inversionInd='true'][count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.118']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:1198-31510) such that it SHALL contain exactly one [1..1] @typeCode="COMP" Component (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:1198-31511). SHALL contain exactly one [1..1] @inversionInd="true" (CONF:1198-31512). SHALL contain exactly one [1..1] Substance Administered Act (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.118) (CONF:1198-31514).</sch:assert>
      <sch:assert id="a-1198-8842" test="not(cda:doseQuantity) or cda:doseQuantity[@unit]">The doseQuantity, if present, SHOULD contain zero or one [0..1] @unit, which SHALL be selected from ValueSet UnitsOfMeasureCaseSensitive urn:oid:2.16.840.1.113883.1.11.12839 DYNAMIC (CONF:1198-8842).</sch:assert>
      <sch:assert id="a-1198-8849" test="count(cda:performer)=1">SHOULD contain zero or one [0..1] performer (CONF:1198-8849).</sch:assert>
      <sch:assert id="a-1198-31151" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31151).</sch:assert>
      <sch:assert id="a-1198-32960" test="not(cda:routeCode) or cda:routeCode[count(cda:translation) &gt; 0]">The routeCode, if present, SHOULD contain zero or more [0..*] translation, which SHALL be selected from ValueSet Medication Route urn:oid:2.16.840.1.113762.1.4.1099.12 DYNAMIC (CONF:1198-32960).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.52' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.52-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7147" test="count(cda:interpretationCode) &gt; 0">SHOULD contain zero or more [0..*] interpretationCode (CONF:1198-7147).</sch:assert>
      <sch:assert id="a-1198-7150" test="count(cda:referenceRange) &gt; 0">SHOULD contain zero or more [0..*] referenceRange (CONF:1198-7150).</sch:assert>
      <sch:assert id="a-1198-7149" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-7149).</sch:assert>
      <sch:assert id="a-1198-32610-c" test="(cda:value[@xsi:type='CD'][@codeSystem='2.16.840.1.113883.6.96']) or not(cda:value[@xsi:type='CD'])">If Observation/value is a CD (**xsi:type="CD"**) the value **SHOULD** be SNOMED-CT (CONF:1198-32610).</sch:assert>
      <sch:assert id="a-1198-7133-v" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:1198-7133).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.2' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-oid-2.16.840.1.113883.10.20.22.4.147-warnings">
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-warnings-abstract" abstract="true">
      <sch:assert id="a-81-32756" test="cda:text/cda:reference[@value]">This reference SHOULD contain zero or one [0..1] @value (CONF:81-32756).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.147']]">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.22.4.147-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings">
    <!--Pattern is used in an implied relationship.-->
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7969" test="count(cda:entry[count(cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.52' and @extension='2015-08-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-7969) such that it SHALL contain exactly one [1..1] Immunization Activity (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.52:2015-08-01) (CONF:1198-15494).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.2.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7983" test="count(cda:entry)=1">SHOULD contain zero or one [0..1] entry (CONF:1198-7983).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.24' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.24-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-31147" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31147).</sch:assert>
      <sch:assert id="a-1198-9045-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Problem Type (SNOMEDCT) urn:oid:2.16.840.1.113883.3.88.12.3221.7.2 DYNAMIC (CONF:1198-9045).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.4' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-8559" test="count(cda:value)=1">SHOULD contain zero or one [0..1] value (CONF:1198-8559).</sch:assert>
      <sch:assert id="a-1198-31869" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31869).</sch:assert>
      <sch:assert id="a-1198-8558-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Social History Type urn:oid:2.16.840.1.113883.3.88.12.80.60 DYNAMIC (CONF:1198-8558).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.38' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.38-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7119" test="count(cda:entry[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.1' and @extension='2015-08-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-7119) such that it SHALL contain exactly one [1..1] Result Organizer (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.1:2015-08-01) (CONF:1198-15515).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-19218-c" test="cda:code[@codeSystem='2.16.840.1.113883.6.1'] or cda:code[@codeSystem='2.16.840.1.113883.6.96'] or cda:code[@codeSystem='2.16.840.1.113883.6.12']">**SHOULD** be selected from LOINC (codeSystem 2.16.840.1.113883.6.1) **OR** SNOMED CT (codeSystem 2.16.840.1.113883.6.96), and **MAY** be selected from CPT-4 (codeSystem 2.16.840.1.113883.6.12) (CONF:1198-19218).</sch:assert>
      <sch:assert id="a-1198-31149" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31149).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.3.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.3.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-10102" test="count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.36' and @extension='2014-06-09']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-10102) such that it SHALL contain exactly one [1..1] Admission Medication (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.36:2014-06-09) (CONF:1198-15484).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.44' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.44-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.33' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.33-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.80' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.80-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.20' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.20-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.34' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.34-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7271" test="count(cda:entry[count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.26' and @extension='2015-08-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-7271) such that it SHALL contain exactly one [1..1] Vital Signs Organizer (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.26:2015-08-01) (CONF:1198-15517).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.4.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7881" test="count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.3' and @extension='2015-08-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-7881) such that it SHALL contain exactly one [1..1] Problem Concern Act (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.3:2015-08-01) (CONF:1198-15505).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-31146" test="count(cda:author[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.119']]) &gt; 0">SHOULD contain zero or more [0..*] Author Participation (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.119) (CONF:1198-31146).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.3' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.3-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.5.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.5.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-14823" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.78' and @extension='2014-06-09']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-14823) such that it SHALL contain exactly one [1..1] Smoking Status - Meaningful Use (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.78:2014-06-09) (CONF:1198-14824).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-5382" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:telecom) &gt; 0]">The guardian, if present, SHOULD contain zero or more [0..*] telecom (CONF:1198-5382).</sch:assert>
      <sch:assert id="a-1198-5406" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:languageCommunication) &gt; 0]">This patient SHOULD contain zero or more [0..*] languageCommunication (CONF:1198-5406).</sch:assert>
      <sch:assert id="a-1198-16787" test="cda:author/cda:assignedAuthor[count(cda:code)=1]">This assignedAuthor SHOULD contain zero or one [0..1] code (CONF:1198-16787).</sch:assert>
      <sch:assert id="a-1198-5430-c" test="not(tested-here)">This assignedAuthor SHOULD contain zero or one [0..1] assignedPerson (CONF:1198-5430).</sch:assert>
      <sch:assert id="a-1198-16783-c" test="not(tested-here)">This assignedAuthor SHOULD contain zero or one [0..1] assignedAuthoringDevice (CONF:1198-16783).</sch:assert>
      <sch:assert id="a-1198-32882-c" test="count(cda:author/cda:assignedAuthor[cda:assignedPerson]) = count(cda:author/cda:assignedAuthor[cda:assignedPerson and cda:id/@root='2.16.840.1.113883.4.6'])">This assignedAuthor SHOULD contain zero or one [0..1] id (CONF:1198-32882) such that it SHALL contain exactly one [1..1] @root="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-32884).</sch:assert>
      <sch:assert id="a-1198-5579" test="count(cda:legalAuthenticator)=1">SHOULD contain zero or one [0..1] legalAuthenticator (CONF:1198-5579).</sch:assert>
      <sch:assert id="a-1198-14839" test="not(cda:documentationOf/cda:serviceEvent) or cda:documentationOf/cda:serviceEvent[count(cda:performer) &gt; 0]">This serviceEvent SHOULD contain zero or more [0..*] performer (CONF:1198-14839).</sch:assert>
      <sch:assert id="a-1198-5375" test="cda:recordTarget/cda:patientRole/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-5375).</sch:assert>
      <sch:assert id="a-1198-5300-c" test="cda:recordTarget/cda:patientRole/cda:patient/cda:birthTime/@nullFlavor or string-length(cda:recordTarget/cda:patientRole/cda:patient/cda:birthTime/@value) &gt;= 8">**SHOULD** be precise to day (CONF:1198-5300).</sch:assert>
      <sch:assert id="a-1198-5303" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:maritalStatusCode)=1]">This patient SHOULD contain zero or one [0..1] maritalStatusCode, which SHALL be selected from ValueSet Marital Status urn:oid:2.16.840.1.113883.1.11.12212 DYNAMIC (CONF:1198-5303).</sch:assert>
      <sch:assert id="a-1198-5326" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian[count(cda:code)=1]">The guardian, if present, SHOULD contain zero or one [0..1] code, which SHALL be selected from ValueSet Personal And Legal Relationship Role Type urn:oid:2.16.840.1.113883.11.20.12.1 DYNAMIC (CONF:1198-5326).</sch:assert>
      <sch:assert id="a-1198-5359-c" test="count( cda:recordTarget/cda:patientRole/cda:patient/cda:guardian) &lt;= count(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:addr)">The guardian, if present, SHOULD contain zero or more [0..*] US Realm Address (AD.US.FIELDED) (identifier: urn:oid:2.16.840.1.113883.10.20.22.5.2) (CONF:1198-5359).</sch:assert>
      <sch:assert id="a-1198-7993" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:telecom) or cda:recordTarget/cda:patientRole/cda:patient/cda:guardian/cda:telecom[@use]">The telecom, if present, SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7993).</sch:assert>
      <sch:assert id="a-1198-5404" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace/cda:place/cda:addr) or cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace/cda:place/cda:addr[count(cda:country)=1]">This addr SHOULD contain zero or one [0..1] country, which SHALL be selected from ValueSet Country urn:oid:2.16.840.1.113883.3.88.12.80.63 DYNAMIC (CONF:1198-5404).</sch:assert>
      <sch:assert id="a-1198-5402-c" test="count(cda:recordTarget/cda:patientRole/cda:patient/cda:birthplace/cda:place/cda:addr[cda:country='US' or cda:country='USA'][count(cda:state)!=1])=0">If country is US, this addr **SHALL** contain exactly one [1..1] state, which **SHALL** be selected from ValueSet StateValueSet 2.16.840.1.113883.3.88.12.80.1 *DYNAMIC* (CONF:1198-5402).</sch:assert>
      <sch:assert id="a-1198-9965" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication) or cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication[count(cda:proficiencyLevelCode)=1]">The languageCommunication, if present, SHOULD contain zero or one [0..1] proficiencyLevelCode, which SHALL be selected from ValueSet LanguageAbilityProficiency urn:oid:2.16.840.1.113883.1.11.12199 DYNAMIC (CONF:1198-9965).</sch:assert>
      <sch:assert id="a-1198-5414" test="not(cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication) or cda:recordTarget/cda:patientRole/cda:patient/cda:languageCommunication[count(cda:preferenceInd)=1]">The languageCommunication, if present, SHOULD contain zero or one [0..1] preferenceInd (CONF:1198-5414).</sch:assert>
      <sch:assert id="a-1198-16820" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:id) or cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:id[@root='2.16.840.1.113883.4.6']">Such ids SHOULD contain zero or one [0..1] @root="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-16820).</sch:assert>
      <sch:assert id="a-1198-7994" test="not(cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:telecom) or cda:recordTarget/cda:patientRole/cda:providerOrganization/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7994).</sch:assert>
      <sch:assert id="a-1198-7995" test="cda:author/cda:assignedAuthor/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7995).</sch:assert>
      <sch:assert id="a-1198-16821" test="not(cda:dataEnterer/cda:assignedEntity/cda:id) or cda:dataEnterer/cda:assignedEntity/cda:id[@root='2.16.840.1.113883.4.6']">Such ids SHOULD contain zero or one [0..1] @root="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-16821).</sch:assert>
      <sch:assert id="a-1198-7996" test="not(cda:dataEnterer/cda:assignedEntity/cda:telecom) or cda:dataEnterer/cda:assignedEntity/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7996).</sch:assert>
      <sch:assert id="a-1198-9946-c" test="not(testable)">If assignedEntity/id is a provider then this id, **SHOULD** include zero or one [0..1] id where id/@root ="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-9946).</sch:assert>
      <sch:assert id="a-1198-16822" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:id[@root='2.16.840.1.113883.4.6']">Such ids SHOULD contain zero or one [0..1] @root="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-16822).</sch:assert>
      <sch:assert id="a-1198-7998" test="cda:custodian/cda:assignedCustodian/cda:representedCustodianOrganization/cda:telecom[@use]">This telecom SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7998).</sch:assert>
      <sch:assert id="a-1198-7999" test="not(cda:legalAuthenticator/cda:assignedEntity/cda:telecom) or cda:legalAuthenticator/cda:assignedEntity/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-7999).</sch:assert>
      <sch:assert id="a-1198-10007-c" test="count(cda:participant[@typeCode='IND']) = count(cda:participant/cda:associatedEntity[@classCode=document('voc.xml')/voc:systems/voc:system[@valueSetOid='2.16.840.1.113883.11.20.9.33']/voc:code/@value])">When participant/@typeCode is *IND*, associatedEntity/@classCode **SHOULD** be selected from ValueSet 2.16.840.1.113883.11.20.9.33 INDRoleclassCodes *STATIC 2011-09-30* (CONF:1198-10007).</sch:assert>
      <sch:assert id="a-1198-32889" test="not(cda:documentationOf/cda:serviceEvent/cda:performer/cda:functionCode) or cda:documentationOf/cda:serviceEvent/cda:performer/cda:functionCode[@code]">The functionCode, if present, SHOULD contain zero or one [0..1] @code, which SHOULD be selected from ValueSet Care Team Member Function urn:oid:2.16.840.1.113762.1.4.1099.30 DYNAMIC (CONF:1198-32889).</sch:assert>
      <sch:assert id="a-1198-14847" test="not(cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:id) or cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity/cda:id[@root='2.16.840.1.113883.4.6']">Such ids SHOULD contain zero or one [0..1] @root="2.16.840.1.113883.4.6" National Provider Identifier (CONF:1198-14847).</sch:assert>
      <sch:assert id="a-1198-14842" test="not(cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity) or cda:documentationOf/cda:serviceEvent/cda:performer/cda:assignedEntity[count(cda:code)=1]">This assignedEntity SHOULD contain zero or one [0..1] code, which SHOULD be selected from ValueSet Healthcare Provider Taxonomy urn:oid:2.16.840.1.114222.4.11.1066 DYNAMIC (CONF:1198-14842).</sch:assert>
      <sch:assert id="a-1198-16788-v" test="not(cda:author/cda:assignedAuthor/cda:code) or cda:author/cda:assignedAuthor/cda:code[@code]">The code, if present, SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Healthcare Provider Taxonomy urn:oid:2.16.840.1.114222.4.11.1066 DYNAMIC (CONF:1198-16788).</sch:assert>
      <sch:assert id="a-1198-5259-v" test="count(cda:confidentialityCode)=1">SHALL contain exactly one [1..1] confidentialityCode, which SHOULD be selected from ValueSet HL7 BasicConfidentialityKind urn:oid:2.16.840.1.113883.1.11.16926 DYNAMIC (CONF:1198-5259).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-32882-branch-32882-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-32885-branch-32882" test="@extension">SHOULD contain zero or one [0..1] @extension (CONF:1198-32885).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-32882-branch-32882-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:author/cda:assignedAuthor/cda:id[@root='2.16.840.1.113883.4.6']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-32882-branch-32882-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-5607-branch-5607-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-16824-branch-5607" test="not(cda:assignedEntity/cda:id) or cda:assignedEntity/cda:id[@root='2.16.840.1.113883.4.6']">Such ids SHOULD contain zero or one [0..1] @root="2.16.840.1.113883.4.6" National Provider Identifier  (CONF:1198-16824).</sch:assert>
      <sch:assert id="a-1198-8000-branch-5607" test="not(cda:assignedEntity/cda:telecom) or cda:assignedEntity/cda:telecom[@use]">Such telecoms SHOULD contain zero or one [0..1] @use, which SHALL be selected from ValueSet Telecom Use (US Realm Header) urn:oid:2.16.840.1.113883.11.20.9.20 DYNAMIC (CONF:1198-8000).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-5607-branch-5607-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.22.1.1' and @extension='2015-08-01']]/cda:authenticator">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-5607-branch-5607-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-8738" test="count(cda:participant[@typeCode='LOC'][count(cda:participantRole[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.32']])=1]) &gt; 0">SHOULD contain zero or more [0..*] participant (CONF:1198-8738) such that it SHALL contain exactly one [1..1] Service Delivery Location (identifier: urn:oid:2.16.840.1.113883.10.20.22.4.32) (CONF:1198-14903). SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90 STATIC) (CONF:1198-8740).</sch:assert>
      <sch:assert id="a-1198-8719" test="cda:code[count(cda:originalText)=1]">This code SHOULD contain zero or one [0..1] originalText (CONF:1198-8719).</sch:assert>
      <sch:assert id="a-1198-15970" test="not(cda:code/cda:originalText) or cda:code/cda:originalText[count(cda:reference)=1]">The originalText, if present, SHOULD contain zero or one [0..1] reference (CONF:1198-15970).</sch:assert>
      <sch:assert id="a-1198-15971" test="not(cda:code/cda:originalText/cda:reference) or cda:code/cda:originalText/cda:reference[@value]">The reference, if present, SHOULD contain zero or one [0..1] @value (CONF:1198-15971).</sch:assert>
      <sch:assert id="a-1198-32177-c" test="not(sdtc:dischargeDispositionCode) or count(sdtc:dischargeDispositionCode[@code])=1">This sdtc:dischargeDispositionCode **SHOULD** contain exactly [0..1] *code*, which **SHOULD** be selected from ValueSet 2.16.840.1.113883.3.88.12.80.33 NUBC UB-04 FL17-Patient Status (code system 2.16.840.1.113883.6.301.5) *DYNAMIC* or, if access to NUBC is unavailable, from CodeSystem 2.16.840.1.113883.12.112 HL7 Discharge Disposition (CONF:1198-32177).</sch:assert>
      <sch:assert id="a-1198-32377-c" test="not(sdtc:dischargeDispositionCode) or (sdtc:dischargeDispositionCode[@codeSystem='2.16.840.1.113883.6.301.5'] or sdtc:dischargeDispositionCode[@codeSystem='2.16.840.1.113883.12.112'])">This sdtc:dischargeDispositionCode **SHOULD** contain exactly [0..1] *codeSystem*, which **SHOULD** be either CodeSystem: NUBC 2.16.840.1.113883.6.301.5 *OR* CodeSystem: HL7 Discharge Disposition 2.16.840.1.113883.12.112 (CONF:1198-32377).</sch:assert>
      <sch:assert id="a-1198-8714-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet EncounterTypeCode urn:oid:2.16.840.1.113883.3.88.12.80.32 DYNAMIC (CONF:1198-8714).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-warnings" context="cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.49' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.49-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings-abstract" abstract="true">
      <sch:assert id="a-1198-7951" test="count(cda:entry[count(cda:encounter[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.49' and @extension='2015-08-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:1198-7951) such that it SHALL contain exactly one [1..1] Encounter Activity (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.49:2015-08-01) (CONF:1198-15465).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.22.1' and @extension='2015-08-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.22.1-2015-08-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.200' and @extension='2016-06-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.200-2016-06-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.34' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.34-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-732" test="cda:value[@xsi:type='CD'][@code]">This value SHOULD contain zero or one [0..1] @code (CONF:3315-732).</sch:assert>
      <sch:assert id="a-3315-733" test="cda:value[@xsi:type='CD'][@codeSystem]">This value SHOULD contain zero or one [0..1] @codeSystem (CONF:3315-733).</sch:assert>
      <sch:assert id="a-3315-734" test="cda:value[@xsi:type='CD'][@displayName]">This value SHOULD contain zero or one [0..1] @displayName (CONF:3315-734).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.12' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.12-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-326" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.14' and @extension='2017-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3315-326) such that it SHALL contain exactly one [1..1] Reporting Timeframe (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.14:2017-04-01) (CONF:3315-330).</sch:assert>
      <sch:assert id="a-3315-328" test="count(cda:component[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.20' and @extension='2017-04-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] component (CONF:3315-328) such that it SHALL contain exactly one [1..1] External Resources (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.20:2017-04-01) (CONF:3315-453).</sch:assert>
      <sch:assert id="a-3315-338" test="count(cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.2' and @extension='2017-04-01']]) &gt; 0">SHOULD contain zero or more [0..*] Responsible Agency (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.2:2017-04-01) (CONF:3315-338).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.13' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.13-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-250-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-250).</sch:assert>
      <sch:assert id="a-3315-672-v" test="cda:value[@xsi:type='PQ'][@unit]">This value SHALL contain exactly one [1..1] @unit, which SHOULD be selected from CodeSystem UCUM (urn:oid:2.16.840.1.113883.6.8) DYNAMIC (CONF:3315-672).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.14' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.14-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-273" test="count(cda:text)=1">SHOULD contain zero or one [0..1] text (CONF:3315-273).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-warnings" context="cda:externalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.17' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.17-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-533" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.26' and @extension='2017-04-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:3315-533) such that it SHALL contain exactly one [1..1] Determination of Reportability Reason (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.26:2017-04-01) (CONF:3315-536).</sch:assert>
      <sch:assert id="a-3315-534" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.27' and @extension='2017-04-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:3315-534) such that it SHALL contain exactly one [1..1] Determination of Reportability Rule (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.27:2017-04-01) (CONF:3315-538).</sch:assert>
      <sch:assert id="a-3315-348-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-348).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.19' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.19-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-394" test="cda:participantRole[count(cda:id) &gt; 0]">This participantRole SHOULD contain zero or more [0..*] id (CONF:3315-394).</sch:assert>
      <sch:assert id="a-3315-397" test="cda:participantRole[count(cda:addr) &gt; 0]">This participantRole SHOULD contain at least one [1..*] addr (CONF:3315-397).</sch:assert>
      <sch:assert id="a-3315-398" test="cda:participantRole[count(cda:telecom) &gt; 0]">This participantRole SHOULD contain at least one [1..*] telecom (CONF:3315-398).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.1' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.1-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-412" test="cda:participantRole[count(cda:id) &gt; 0]">This participantRole SHOULD contain zero or more [0..*] id (CONF:3315-412).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.2' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.2-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-425" test="cda:participantRole[count(cda:id) &gt; 0]">This participantRole SHOULD contain zero or more [0..*] id (CONF:3315-425).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.3' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.3-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.20' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.20-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-515-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-515).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.26' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.26-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3315-525-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3315-525).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.27' and @extension='2017-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.27-2017-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert id="a-3349-20" test="cda:effectiveTime[count(cda:high)=1]">This effectiveTime SHOULD contain zero or one [0..1] high (CONF:3349-20).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.212' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.212-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.220' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.220-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert id="a-3349-67-v" test="count(cda:value[@xsi:type='TS'])=1">SHALL contain exactly one [1..1] value with @xsi:type="TS" (CONF:3349-67).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.218' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.218-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.213' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.213-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.214' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.214-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.215' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.215-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.222' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.222-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert id="a-3349-292" test="cda:value[@xsi:type='PQ'][@unit='h/d']">This value SHOULD contain zero or one [0..1] @unit="h/d" hours per day (CodeSystem: UCUM urn:oid:2.16.840.1.113883.6.8) (CONF:3349-292).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.211' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.211-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert id="a-3349-135" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.211' and @extension='2017-11-30']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:3349-135) such that it SHALL contain exactly one [1..1] Daily Work Hours Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.211:2017-11-30) (CONF:3349-136).</sch:assert>
      <sch:assert id="a-3349-137" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.222' and @extension='2017-11-30']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:3349-137) such that it SHALL contain exactly one [1..1] Weekly Work Days Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.222:2017-11-30) (CONF:3349-138).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.223-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.210' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.210-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-warnings-abstract" abstract="true">
      <sch:assert id="a-3349-280-v" test="count(cda:value)=1">SHALL contain exactly one [1..1] value, which SHOULD be selected from ValueSet Job Supervisory Level or Pay Grade (ODH) urn:oid:2.16.840.1.114222.4.11.7613 DYNAMIC (CONF:3349-280).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.224' and @extension='2017-11-30']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.224-2017-11-30-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.80' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.80-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26544" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26544).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.280' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.280-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.281' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.281-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.282' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.282-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26578-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Other Pregnancy Outcome urn:oid:2.16.840.1.113883.11.20.9.84 DYNAMIC (CONF:3368-26578).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.291' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.291-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.284' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.284-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26613-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Postpartum Status urn:oid:2.16.840.1.113883.11.20.9.87 DYNAMIC (CONF:3368-26613).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.285' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.285-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.286' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.286-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26710" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.282' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26710) such that it SHALL contain exactly one [1..1] Gravidity (Total Pregnancies) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.282:2018-04-01) (CONF:3368-26711).</sch:assert>
      <sch:assert id="a-3368-26712" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.283' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26712) such that it SHALL contain exactly one [1..1] Parity (Total Pregnancies Reaching 20 Weeks) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.283:2018-04-01) (CONF:3368-26713).</sch:assert>
      <sch:assert id="a-3368-26714" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.290' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26714) such that it SHALL contain exactly one [1..1] Living Children (Number of Living Children) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.290:2018-04-01) (CONF:3368-26715).</sch:assert>
      <sch:assert id="a-3368-26716" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.289' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26716) such that it SHALL contain exactly one [1..1] Preterm (Total Pregnancies Delivered Between 20 and 37 Weeks) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.289:2018-04-01) (CONF:3368-26717).</sch:assert>
      <sch:assert id="a-3368-26718" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.288' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26718) such that it SHALL contain exactly one [1..1] Term (Total Pregnancies Delivered Past 37 Weeks) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.288:2018-04-01) (CONF:3368-26719).</sch:assert>
      <sch:assert id="a-3368-26720" test="count(cda:component[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.287' and @extension='2018-04-01']])=1])=1">SHOULD contain zero or one [0..1] component (CONF:3368-26720) such that it SHALL contain exactly one [1..1] Aborta (Total Pregnancies Delivered Before 20 Weeks) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.287:2018-04-01) (CONF:3368-26721).</sch:assert>
      <sch:assert id="a-3368-26818" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26818).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.292' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.292-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.283' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.283-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.287' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.287-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.288' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.288-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.289' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.289-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.290' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.290-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26737" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26737).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.294' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.294-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26751" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26751).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.295' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.295-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26763" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26763).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.296' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.296-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-oid-2.16.840.1.113883.10.20.15.3.8-warnings-abstract" />
      <sch:assert id="a-3368-26776" test="count(cda:performer[count(cda:time)=1])=1">SHOULD contain zero or one [0..1] performer (CONF:3368-26776) such that it SHALL contain exactly one [1..1] time (CONF:3368-26777).</sch:assert>
      <sch:assert id="a-3368-26778" test="count(cda:author[count(cda:time)=1])=1">SHOULD contain zero or one [0..1] author (CONF:3368-26778) such that it SHALL contain exactly one [1..1] time (CONF:3368-26779).</sch:assert>
      <sch:assert id="a-3368-26801" test="count(cda:methodCode)=1">SHOULD contain zero or one [0..1] methodCode, which SHALL be selected from ValueSet Pregnancy Status Determination Method urn:oid:2.16.840.1.113883.11.20.9.80 DYNAMIC (CONF:3368-26801).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.293' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.293-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3368-26813" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3368-26813).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.297' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.297-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3378-329" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:3378-329).</sch:assert>
      <sch:assert id="a-3378-330" test="count(cda:targetSiteCode)=1">SHOULD contain zero or one [0..1] targetSiteCode, which SHALL be selected from ValueSet Body Site Value Set urn:oid:2.16.840.1.113883.3.88.12.3221.8.9 DYNAMIC (CONF:3378-330).</sch:assert>
      <sch:assert id="a-3378-452" test="count(cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.410' and @extension='2018-09-01']])=1">SHOULD contain zero or one [0..1] Specimen Participant (ID) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.410:2018-09-01) (CONF:3378-452).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.415' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.415-2018-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-warnings-abstract" abstract="true">
      <sch:assert id="a-3378-375-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-375).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.418' and @extension='2018-06-11']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.418-2018-06-11-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3378-381-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Observation Result Status Codes Interpretation urn:oid:2.16.840.1.113883.21.38 DYNAMIC (CONF:3378-381).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.419' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.419-2018-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3378-393-v" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:3378-393).</sch:assert>
      <sch:assert id="a-3378-394-v" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:3378-394).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.417' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings-abstract" />
      <sch:assert id="a-3378-433-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Specimen Reject Reason urn:oid:2.16.840.1.113883.21.330 DYNAMIC (CONF:3378-433).</sch:assert>
      <sch:assert id="a-3378-436-v" test="count(cda:code[@codeSystem='2.16.840.1.113883.6.1' or @nullFlavor])=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from CodeSystem LOINC (urn:oid:2.16.840.1.113883.6.1) (CONF:3378-436).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.420' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.420-2018-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.417-2018-09-01-warnings-abstract" />
      <sch:assert id="a-3378-443-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet HL7 Specimen Condition urn:oid:2.16.840.1.113883.21.333 DYNAMIC (CONF:3378-443).</sch:assert>
      <sch:assert id="a-3378-449-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:3378-449).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.421' and @extension='2018-06-12']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.421-2018-06-12-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-3378-460-v" test="cda:participantRole/cda:playingEntity[count(cda:code)=1]">This playingEntity SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet HL7 Specimen Type urn:oid:2.16.840.1.113883.21.327 DYNAMIC (CONF:3378-460).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.410' and @extension='2018-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.410-2018-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.298' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.298-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings-abstract" />
      <sch:assert id="a-3368-26859-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Delivery (NCHS) urn:oid:1.3.6.1.4.1.19376.1.7.3.1.1.13.8.14 DYNAMIC (CONF:3368-26859).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.299' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.299-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings-abstract" />
      <sch:assert id="a-3368-26899-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet D(Rh) Type urn:oid:2.16.840.1.113883.11.20.9.89 DYNAMIC (CONF:3368-26899).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.300' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.300-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.302' and @extension='2018-08-31']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-08-31-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.16-2014-06-09-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-warnings" context="cda:substanceAdministration[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.302' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.302-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-warnings" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.303' and @extension='2018-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.303-2018-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.2-2015-08-01-warnings-abstract" />
      <sch:assert id="a-4411-290" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4411-290).</sch:assert>
      <sch:assert id="a-4411-291" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4411-291).</sch:assert>
      <sch:assert id="a-4411-305-c" test="not(tested_eicr_stu_comment_1694)">This code SHOULD contain zero or one [0..1] @code, which SHOULD be selected from ValueSet Lab Obs Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1057 DYNAMIC (CONF:4411-305).</sch:assert>
      <sch:assert id="a-4411-463" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHOULD contain zero or one [0..1] @code, which MAY be selected from ValueSet Lab Obs Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1057 DYNAMIC (CONF:4411-463).</sch:assert>
      <sch:assert id="a-4411-273-v" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:4411-273).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.2' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.2-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.1-2015-08-01-warnings-abstract" />
      <sch:assert id="a-4411-439" test="cda:code[@sdtc:valueSet]">This code SHOULD contain zero or one [0..1] @sdtc:valueSet (CONF:4411-439).</sch:assert>
      <sch:assert id="a-4411-440" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4411-440).</sch:assert>
      <sch:assert id="a-4411-467" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHOULD contain zero or one [0..1] @code, which MAY be selected from ValueSet Lab Obs Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1057 DYNAMIC (CONF:4411-467).</sch:assert>
      <sch:assert id="a-4411-438-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Lab Obs Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1057 DYNAMIC (CONF:4411-438).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.35' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.35-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.34.3.45' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.34.3.45-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4411-471" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4411-471).</sch:assert>
      <sch:assert id="a-4411-338" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4411-338).</sch:assert>
      <sch:assert id="a-4411-470-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Lab Order Test Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.1056 DYNAMIC (CONF:4411-470).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.4' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.4-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.23-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4411-528" test="cda:manufacturedMaterial/cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4411-528).</sch:assert>
      <sch:assert id="a-4411-529" test="cda:manufacturedMaterial/cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4411-529).</sch:assert>
      <sch:assert id="a-4411-537-v" test="cda:manufacturedMaterial/cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-537).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-warnings" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.36' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.36-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.6' and @extension='2019-06-20']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.6-2019-06-20-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4411-570" test="count(cda:value[@xsi:type='CD'])=1">SHOULD contain zero or one [0..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Therapeutic Response to Medication urn:oid:2.16.840.1.113883.10.20.15.2.5.12 DYNAMIC (CONF:4411-570).</sch:assert>
      <sch:assert id="a-4411-569-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4411-569).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.37' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.37-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" />
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.5' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.5-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.54-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4411-589" test="cda:manufacturedMaterial/cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4411-589).</sch:assert>
      <sch:assert id="a-4411-590" test="cda:manufacturedMaterial/cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4411-590).</sch:assert>
      <sch:assert id="a-4411-591-v" test="cda:manufacturedMaterial/cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Medications Triggers for Public Health Reporting (RCTC Subset) urn:oid:2.16.840.1.113762.1.4.1146.1060 DYNAMIC (CONF:4411-591).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-warnings" context="cda:manufacturedProduct[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.38' and @extension='2019-04-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.38-2019-04-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4480-315" test="cda:value[@xsi:type='CD'][count(cda:translation)=1]">This value SHOULD contain zero or one [0..1] translation, which SHALL be selected from ValueSet Industry NAICS Detail (ODH) urn:oid:2.16.840.1.114222.4.11.7900 (CONF:4480-315).</sch:assert>
      <sch:assert id="a-4480-45-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Industry CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7187 DYNAMIC (CONF:4480-45).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.219' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.219-2020-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4480-36" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.220' and @extension='2017-11-30']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:4480-36) such that it SHALL contain exactly one [1..1] Usual Occupation Duration Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.220:2017-11-30) (CONF:4480-100).</sch:assert>
      <sch:assert id="a-4480-101" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.219' and @extension='2020-09-01']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:4480-101) such that it SHALL contain exactly one [1..1] Usual Industry Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.219:2020-09-01) (CONF:4480-102).</sch:assert>
      <sch:assert id="a-4480-314" test="cda:value[@xsi:type='CD'][count(cda:translation)=1]">This value SHOULD contain zero or one [0..1] translation, which SHALL be selected from ValueSet Occupation ONETSOC Detail (ODH) urn:oid:2.16.840.1.114222.4.11.7901 DYNAMIC (CONF:4480-314).</sch:assert>
      <sch:assert id="a-4480-34-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Occupation CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7186 DYNAMIC (CONF:4480-34).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.221-2020-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4480-146" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.223' and @extension='2017-11-30']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:4480-146) such that it SHALL contain exactly one [1..1] Work Schedule Observation  (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.223:2017-11-30) (CONF:4480-147).</sch:assert>
      <sch:assert id="a-4480-248" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.210' and @extension='2017-11-30']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:4480-248) such that it SHALL contain exactly one [1..1] Work Classification Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.210:2017-11-30) (CONF:4480-249).</sch:assert>
      <sch:assert id="a-4480-281" test="count(cda:entryRelationship[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.224' and @extension='2017-11-30']])=1])=1">SHOULD contain zero or one [0..1] entryRelationship (CONF:4480-281) such that it SHALL contain exactly one [1..1] Supervisory Level Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.224:2017-11-30) (CONF:4480-282).</sch:assert>
      <sch:assert id="a-4480-164" test="cda:participant/cda:participantRole[count(cda:id)=1]">This participantRole SHOULD contain zero or one [0..1] id (CONF:4480-164).</sch:assert>
      <sch:assert id="a-4480-230" test="not(cda:participant/cda:participantRole/cda:playingEntity) or cda:participant/cda:participantRole/cda:playingEntity[count(cda:name)=1]">The playingEntity, if present, SHOULD contain zero or one [0..1] name (CONF:4480-230).</sch:assert>
      <sch:assert id="a-4480-229" test="cda:participant/cda:participantRole[count(cda:addr)=1]">This participantRole SHOULD contain zero or one [0..1] addr (CONF:4480-229).</sch:assert>
      <sch:assert id="a-4480-316" test="cda:value[@xsi:type='CD'][count(cda:translation)=1]">This value SHOULD contain zero or one [0..1] translation, which SHALL be selected from ValueSet Occupation ONETSOC Detail (ODH) urn:oid:2.16.840.1.114222.4.11.7901 (CONF:4480-316).</sch:assert>
      <sch:assert id="a-4480-160-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Occupation CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7186 DYNAMIC (CONF:4480-160).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.217-2020-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4480-317" test="cda:value[@xsi:type='CD'][count(cda:translation)=1]">This value SHOULD contain zero or one [0..1] translation, which SHALL be selected from ValueSet Industry NAICS Detail (ODH) urn:oid:2.16.840.1.114222.4.11.7900 (CONF:4480-317).</sch:assert>
      <sch:assert id="a-4480-176-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Industry CDC Census 2010 urn:oid:2.16.840.1.114222.4.11.7187 DYNAMIC (CONF:4480-176).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.216' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.216-2020-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2015-08-01-warnings-abstract" />
      <sch:assert id="a-4480-304" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.212' and @extension='2017-11-30']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:4480-304) such that it SHALL contain exactly one [1..1] History of Employment Status Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.212:2017-11-30) (CONF:4480-305).</sch:assert>
      <sch:assert id="a-4480-306" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.221' and @extension='2020-09-01']])=1])=1">SHOULD contain zero or one [0..1] entry (CONF:4480-306) such that it SHALL contain exactly one [1..1] Usual Occupation Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.221:2020-09-01) (CONF:4480-307).</sch:assert>
      <sch:assert id="a-4480-308" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.213' and @extension='2017-11-30']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:4480-308) such that it SHALL contain exactly one [1..1] Combat Zone Period Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.213:2017-11-30) (CONF:4480-309).</sch:assert>
      <sch:assert id="a-4480-310" test="count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.22.4.217' and @extension='2020-09-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entry (CONF:4480-310) such that it SHALL contain exactly one [1..1] Past or Present Occupation Observation (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.4.217:2020-09-01) (CONF:4480-311).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2020-09-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.2.17-2020-09-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.1.1-2015-08-01-warnings-abstract" />
      <sch:assert id="a-4482-6" test="cda:componentOf/cda:encompassingEncounter[count(cda:responsibleParty)=1]">This encompassingEncounter SHOULD contain zero or one [0..1] responsibleParty (CONF:4482-6).</sch:assert>
      <sch:assert id="a-4482-11" test="cda:componentOf/cda:encompassingEncounter[count(cda:location)=1]">This encompassingEncounter SHOULD contain zero or one [0..1] location (CONF:4482-11).</sch:assert>
      <sch:assert id="a-4482-148" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.2.1' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-148) such that it SHALL contain exactly one [1..1] Immunizations Section (entries required) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.2.1:2015-08-01) (CONF:4482-149).</sch:assert>
      <sch:assert id="a-4482-336" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.10' and @extension='2014-06-09']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-336) such that it SHALL contain exactly one [1..1] Plan of Treatment Section (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.10:2014-06-09) (CONF:4482-337).</sch:assert>
      <sch:assert id="a-4482-411" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.4.1' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-411) such that it SHALL contain exactly one [1..1] Vital Signs Section (entries required) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.4.1:2015-08-01) (CONF:4482-412).</sch:assert>
      <sch:assert id="a-4482-452" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.80' and @extension='2018-04-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-452) such that it SHALL contain exactly one [1..1] Pregnancy Section (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.80:2018-04-01) (CONF:4482-453).</sch:assert>
      <sch:assert id="a-4482-454" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.17' and @extension='2020-09-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-454) such that it SHALL contain exactly one [1..1] Occupational Data for Health Template Requirements Section (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.17:2020-09-01) (CONF:4482-455).</sch:assert>
      <sch:assert id="a-4482-110" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:guardian) &gt; 0]">This patient SHOULD contain zero or more [0..*] guardian (CONF:4482-110).</sch:assert>
      <sch:assert id="a-4482-21" test="cda:componentOf/cda:encompassingEncounter/cda:effectiveTime[count(cda:high)=1]">This effectiveTime SHOULD contain zero or one [0..1] high (CONF:4482-21).</sch:assert>
      <sch:assert id="a-4482-548" test="cda:recordTarget/cda:patientRole/cda:patient[count(cda:birthplace)=1]">This patient SHOULD contain zero or one [0..1] birthplace (CONF:4482-548).</sch:assert>
      <sch:assert id="a-4482-545" test="cda:recordTarget/cda:patientRole/cda:addr[count(cda:county)=1]">Such addrs SHOULD contain zero or one [0..1] county, which SHOULD be selected from ValueSet County urn:oid:2.16.840.1.114222.4.11.829 DYNAMIC (CONF:4482-545).</sch:assert>
      <sch:assert id="a-4482-600" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.7.1' and @extension='2014-06-09']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-600) such that it SHALL contain exactly one [1..1] Procedures Section (entries required) (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.7.1:2014-06-09) (CONF:4482-601).</sch:assert>
      <sch:assert id="a-4482-602" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.43' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-602) such that it SHALL contain exactly one [1..1] Admission Diagnosis Section (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.43:2015-08-01) (CONF:4482-603).</sch:assert>
      <sch:assert id="a-4482-604" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='1.3.6.1.4.1.19376.1.5.3.1.3.18']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-604) such that it SHALL contain exactly one [1..1] Review of Systems Section (identifier: urn:oid:1.3.6.1.4.1.19376.1.5.3.1.3.18) (CONF:4482-605).</sch:assert>
      <sch:assert id="a-4482-606" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.20' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-606) such that it SHALL contain exactly one [1..1] Past Medical History (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.20:2015-08-01) (CONF:4482-607).</sch:assert>
      <sch:assert id="a-4482-608" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.24' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-608) such that it SHALL contain exactly one [1..1] Discharge Diagnosis Section (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.24:2015-08-01) (CONF:4482-609).</sch:assert>
      <sch:assert id="a-4482-636" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.4' and @extension='2021-01-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-636) such that it SHALL contain exactly one [1..1] Emergency Outbreak Information Section (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.2.4:2021-01-01) (CONF:4482-637).</sch:assert>
      <sch:assert id="a-4482-1060" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.44' and @extension='2015-08-01']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-1060) such that it SHALL contain exactly one [1..1] Admission Medications Section (entries optional) (V3) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.44:2015-08-01) (CONF:4482-1061).</sch:assert>
      <sch:assert id="a-4482-1062" test="cda:component/cda:structuredBody[count(cda:component[count(cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.22.2.1.1' and @extension='2014-06-09']])=1])=1]">This structuredBody SHOULD contain zero or one [0..1] component (CONF:4482-1062) such that it SHALL contain exactly one [1..1] Medications Section (entries required) (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.22.2.1.1:2014-06-09) (CONF:4482-1063).</sch:assert>
      <sch:assert id="a-4482-1130" test="cda:author/cda:assignedAuthor[count(cda:assignedPerson)=1]">This assignedAuthor SHOULD contain zero or one [0..1] assignedPerson (CONF:4482-1130).</sch:assert>
      <sch:assert id="a-4482-1131" test="cda:author/cda:assignedAuthor[count(cda:representedOrganization)=1]">This assignedAuthor SHOULD contain zero or one [0..1] representedOrganization (CONF:4482-1131).</sch:assert>
      <sch:assert id="a-4482-14-v" test="not(cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility) or cda:componentOf/cda:encompassingEncounter/cda:location/cda:healthCareFacility[count(cda:code)=1]">This healthCareFacility SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ServiceDeliveryLocationRoleType urn:oid:2.16.840.1.113883.1.11.17660 DYNAMIC (CONF:4482-14).</sch:assert>
      <sch:assert id="a-4482-4-v" test="cda:componentOf/cda:encompassingEncounter[count(cda:code)=1]">This encompassingEncounter SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ActEncounterCode urn:oid:2.16.840.1.113883.1.11.13955 DYNAMIC (CONF:4482-4).</sch:assert>
      <sch:assert id="a-4482-398-v" test="not(cda:documentationOf/cda:serviceEvent) or cda:documentationOf/cda:serviceEvent[count(cda:code)=1]">This serviceEvent SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet eICR Initiation urn:oid:2.16.840.1.113883.10.20.15.2.5.11 DYNAMIC (CONF:4482-398).</sch:assert>
      <sch:assert id="a-4482-1043-v" test="not(cda:participant/cda:associatedEntity) or cda:participant/cda:associatedEntity[count(cda:code)=1]">This associatedEntity SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet ServiceDeliveryLocationRoleType urn:oid:2.16.840.1.113883.1.11.17660 DYNAMIC (CONF:4482-1043).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-148-branch-148-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1118-branch-148" test="not(cda:section) or cda:section[count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.55' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1118) such that it SHALL contain exactly one [1..1] Vaccine Credential Patient Assertion (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.55:2021-01-01) (CONF:4482-1119).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-148-branch-148-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:component/cda:structuredBody/cda:component[cda:section]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-148-branch-148-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-336-branch-336-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1092-branch-336" test="not(cda:section) or cda:section[count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.41' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1092) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Planned Act (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.41:2021-01-01) (CONF:4482-1093).</sch:assert>
      <sch:assert id="a-4482-1094-branch-336" test="not(cda:section) or cda:section[count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.4' and @extension='2019-04-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1094) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Lab Test Order (V2) (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.4:2019-04-01) (CONF:4482-1095).</sch:assert>
      <sch:assert id="a-4482-1096-branch-336" test="not(cda:section) or cda:section[count(cda:entry[count(cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.42' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1096) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Planned Procedure (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.42:2021-01-01) (CONF:4482-1097).</sch:assert>
      <sch:assert id="a-4482-1098-branch-336" test="not(cda:section) or cda:section[count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.43' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1098) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Planned Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.43:2021-01-01) (CONF:4482-1099).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-336-branch-336-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:component/cda:structuredBody/cda:component[cda:section]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-336-branch-336-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-600-branch-600-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1100-branch-600" test="not(cda:section) or cda:section[count(cda:entry[count(cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.45' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1100) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Procedure Activity Act (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.45:2021-01-01) (CONF:4482-1101).</sch:assert>
      <sch:assert id="a-4482-1102-branch-600" test="not(cda:section) or cda:section[count(cda:entry[count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.46' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1102) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Procedure Activity Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.46:2021-01-01) (CONF:4482-1103).</sch:assert>
      <sch:assert id="a-4482-1104-branch-600" test="not(cda:section) or cda:section[count(cda:entry[count(cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.44' and @extension='2021-01-01']])=1]) &gt; 0]">This section SHOULD contain zero or more [0..*] entry (CONF:4482-1104) such that it SHALL contain exactly one [1..1] Initial Case Report Trigger Code Procedure Activity Procedure (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.44:2021-01-01) (CONF:4482-1105).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-600-branch-600-warnings" context="cda:ClinicalDocument[cda:templateId[@root='2.16.840.1.113883.10.20.15.2' and @extension='2021-01-01']]/cda:component/cda:structuredBody/cda:component[cda:section]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2-2021-01-01-600-branch-600-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.4' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.4-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-629" test="count(cda:id)=1">SHOULD contain zero or one [0..1] id (CONF:4482-629).</sch:assert>
      <sch:assert id="a-4482-632" test="count(cda:value)=1">SHOULD contain zero or one [0..1] value (CONF:4482-632).</sch:assert>
      <sch:assert id="a-4482-624-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code (CONF:4482-624).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.40' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.40-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.39-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-659" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-659).</sch:assert>
      <sch:assert id="a-4482-660" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-660).</sch:assert>
      <sch:assert id="a-4482-658-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Reportable Condition Trigger Codes (RCTC) urn:oid:2.16.840.1.114222.4.11.7508 DYNAMIC (CONF:4482-658).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.41' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.41-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.41-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-685" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-685).</sch:assert>
      <sch:assert id="a-4482-686" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-686).</sch:assert>
      <sch:assert id="a-4482-684-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Reportable Condition Trigger Codes (RCTC) urn:oid:2.16.840.1.114222.4.11.7508 DYNAMIC (CONF:4482-684).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.42' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.42-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.44-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-714" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-714).</sch:assert>
      <sch:assert id="a-4482-715" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-715).</sch:assert>
      <sch:assert id="a-4482-716-v" test="not(cda:code/cda:translation) or cda:code/cda:translation[@code]">The translation, if present, SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-716).</sch:assert>
      <sch:assert id="a-4482-713-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-713).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.43' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.43-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.4-2015-08-01-warnings-abstract" />
      <sch:assert id="a-4482-187" test="cda:value[@xsi:type='CD'][@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This value SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-187).</sch:assert>
      <sch:assert id="a-4482-188" test="cda:value[@xsi:type='CD'][@sdtc:valueSetVersion]">This value SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-188).</sch:assert>
      <sch:assert id="a-4482-176-v" test="cda:value[@xsi:type='CD'][@code]">This value SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-176).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.3' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.3-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.12-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-881" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-881).</sch:assert>
      <sch:assert id="a-4482-882" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-882).</sch:assert>
      <sch:assert id="a-4482-863-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-863).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.45' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.45-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.13-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-877" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-877).</sch:assert>
      <sch:assert id="a-4482-878" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-878).</sch:assert>
      <sch:assert id="a-4482-861-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-861).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.46' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.46-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-warnings-abstract" abstract="true">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.22.4.14-2014-06-09-warnings-abstract" />
      <sch:assert id="a-4482-873" test="cda:code[@sdtc:valueSet='2.16.840.1.114222.4.11.7508']">This code SHOULD contain zero or one [0..1] @sdtc:valueSet="2.16.840.1.114222.4.11.7508" (CONF:4482-873).</sch:assert>
      <sch:assert id="a-4482-874" test="cda:code[@sdtc:valueSetVersion]">This code SHOULD contain zero or one [0..1] @sdtc:valueSetVersion (CONF:4482-874).</sch:assert>
      <sch:assert id="a-4482-865-v" test="cda:code[@code]">This code SHALL contain exactly one [1..1] @code, which SHOULD be selected from ValueSet Diagnosis_Problem Triggers for Public Health Reporting (RCTC subset) urn:oid:2.16.840.1.113762.1.4.1146.627 DYNAMIC (CONF:4482-865).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-warnings" context="cda:procedure[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.44' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.44-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-warnings" context="cda:section[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.2.5' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.2.5-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-906-v" test="count(cda:value[@xsi:type='BL'])=1">SHALL contain exactly one [1..1] value with @xsi:type="BL" (CONF:4482-906).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.47' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.47-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-918-v" test="count(cda:value[@xsi:type='BL'])=1">SHALL contain exactly one [1..1] value with @xsi:type="BL" (CONF:4482-918).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.48' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.48-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-930-v" test="count(cda:value)=1">SHALL contain exactly one [1..1] value (CONF:4482-930).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.49' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.49-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-952" test="count(cda:component) &gt; 0">SHOULD contain zero or more [0..*] component (CONF:4482-952).</sch:assert>
      <sch:assert id="a-4482-951-v" test="count(cda:code)=1">SHALL contain exactly one [1..1] code, which SHOULD be selected from ValueSet Transport vehicle type urn:oid:2.16.840.1.113762.1.4.1099.50 DYNAMIC (CONF:4482-951).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-warnings" context="cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.50' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.50-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-257" test="count(cda:participant[@typeCode='LOC'][count(cda:participantRole[@classCode='TERR'])=1]) &gt; 0">SHOULD contain zero or more [0..*] participant (CONF:4482-257) such that it SHALL contain exactly one [1..1] participantRole (CONF:4482-262). This participantRole SHALL contain exactly one [1..1] @classCode="TERR" Territory (CodeSystem: HL7RoleClass urn:oid:2.16.840.1.113883.5.110) (CONF:4482-265). SHALL contain exactly one [1..1] @typeCode="LOC" Location (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-258).</sch:assert>
      <sch:assert id="a-4482-956" test="count(cda:entryRelationship[@typeCode='COMP'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.51' and @extension='2021-01-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:4482-956) such that it SHALL contain exactly one [1..1] @typeCode="COMP" Has component (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4482-958). SHALL contain exactly one [1..1] Purpose of Travel Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.51:2021-01-01) (CONF:4482-971).</sch:assert>
      <sch:assert id="a-4482-972" test="count(cda:entryRelationship[@typeCode='COMP'][count(cda:organizer[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.50' and @extension='2021-01-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:4482-972) such that it SHALL contain exactly one [1..1] Transportation Details Organizer (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.50:2021-01-01) (CONF:4482-973). SHALL contain exactly one [1..1] @typeCode="COMP" Has component (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4482-974).</sch:assert>
      <sch:assert id="a-4482-1022" test="count(cda:entryRelationship[@typeCode='COMP'][count(cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.52' and @extension='2021-01-01']])=1]) &gt; 0">SHOULD contain zero or more [0..*] entryRelationship (CONF:4482-1022) such that it SHALL contain exactly one [1..1] Exposure/Contact Information Observation (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.3.52:2021-01-01) (CONF:4482-1023). SHALL contain exactly one [1..1] @typeCode="COMP" Has component (CodeSystem: HL7ActRelationshipType urn:oid:2.16.840.1.113883.5.1002) (CONF:4482-1024).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.1' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-264-branch-257" test="not(cda:participantRole) or cda:participantRole[count(cda:addr)=1]">This participantRole SHOULD contain zero or one [0..1] addr (CONF:4482-264).</sch:assert>
      <sch:assert id="a-4482-267-branch-257" test="not(cda:participantRole/cda:addr) or cda:participantRole/cda:addr[count(cda:state)=1]">The addr, if present, SHOULD contain zero or one [0..1] state, which MAY be selected from ValueSet StateValueSet urn:oid:2.16.840.1.113883.3.88.12.80.1 DYNAMIC (CONF:4482-267).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-warnings" context="cda:act[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.1' and @extension='2021-01-01']]/cda:participant[cda:participantRole[@classCode='TERR']][@typeCode='LOC']">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.1-2021-01-01-257-branch-257-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-967" test="count(cda:id)=1">SHOULD contain zero or one [0..1] id (CONF:4482-967).</sch:assert>
      <sch:assert id="a-4482-968-v" test="count(cda:value[@xsi:type='CD'])=1">SHALL contain exactly one [1..1] value with @xsi:type="CD", where the code SHOULD be selected from ValueSet Travel Purpose urn:oid:2.16.840.1.114222.4.11.3108 DYNAMIC (CONF:4482-968).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.51' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.51-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-999" test="count(cda:value) &gt; 0">SHOULD contain zero or more [0..*] value (CONF:4482-999).</sch:assert>
      <sch:assert id="a-4482-1005" test="count(cda:participant[(cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.4' and @extension='2021-01-01']) or (cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.6' and @extension='2021-01-01']) or (cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.5' and @extension='2021-01-01'])][count(cda:templateId)=1]) &gt; 0">SHOULD contain zero or more [0..*] Location Participant (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.4:2021-01-01) or Person Participant (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.6:2021-01-01) or Animal Participant (identifier: urn:hl7ii:2.16.840.1.113883.10.20.15.2.4.5:2021-01-01) (CONF:4482-1005) such that it SHALL contain exactly one [1..1] templateId (CONF:4482-1129).</sch:assert>
      <sch:assert id="a-4482-1120" test="count(cda:participant[@typeCode='CSM'][count(cda:participantRole)=1]) &gt; 0">SHOULD contain zero or more [0..*] participant (CONF:4482-1120) such that it SHALL contain exactly one [1..1] @typeCode="CSM" consumable (CodeSystem: HL7ParticipationType urn:oid:2.16.840.1.113883.5.90) (CONF:4482-1121). SHALL contain exactly one [1..1] participantRole (CONF:4482-1124).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.52' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-warnings-abstract" />
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-1120-branch-1120-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1126-branch-1120" test="not(cda:participantRole/cda:playingEntity) or cda:participantRole/cda:playingEntity[count(cda:code)=1]">The playingEntity, if present, SHOULD contain zero or one [0..1] code (CONF:4482-1126).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-1120-branch-1120-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.52' and @extension='2021-01-01']]/cda:participant[@typeCode='CSM'][cda:participantRole]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.52-2021-01-01-1120-branch-1120-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-992" test="cda:participantRole[count(cda:addr)=1]">This participantRole SHOULD contain zero or one [0..1] addr (CONF:4482-992).</sch:assert>
      <sch:assert id="a-4482-1003" test="not(cda:participantRole/cda:addr) or cda:participantRole/cda:addr[count(cda:state)=1]">The addr, if present, SHOULD contain zero or one [0..1] state, which MAY be selected from ValueSet StateValueSet urn:oid:2.16.840.1.113883.3.88.12.80.1 DYNAMIC (CONF:4482-1003).</sch:assert>
      <sch:assert id="a-4482-1135" test="count(sdtc:functionCode)=1">SHOULD contain zero or one [0..1] sdtc:functionCode (CONF:4482-1135).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.4' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.4-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.5' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.5-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert test="."></sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-warnings" context="cda:participant[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.4.6' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.4.6-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1072" test="count(cda:id)=1">SHOULD contain zero or one [0..1] id (CONF:4482-1072).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.53' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.53-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1083" test="count(cda:id)=1">SHOULD contain zero or one [0..1] id (CONF:4482-1083).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.54' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.54-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
  <sch:pattern id="p-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-warnings">
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-warnings-abstract" abstract="true">
      <sch:assert id="a-4482-1114" test="count(cda:id)=1">SHOULD contain zero or one [0..1] id (CONF:4482-1114).</sch:assert>
      <sch:assert id="a-4482-1117" test="count(cda:effectiveTime)=1">SHOULD contain zero or one [0..1] effectiveTime (CONF:4482-1117).</sch:assert>
    </sch:rule>
    <sch:rule role="warning" id="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-warnings" context="cda:observation[cda:templateId[@root='2.16.840.1.113883.10.20.15.2.3.55' and @extension='2021-01-01']]">
      <sch:extends rule="r-urn-hl7ii-2.16.840.1.113883.10.20.15.2.3.55-2021-01-01-warnings-abstract" />
    </sch:rule>
  </sch:pattern>
</sch:schema>
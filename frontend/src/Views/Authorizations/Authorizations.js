import React, { Component } from 'react';
import {
  Alert,
  Row,
  Col
} from 'react-bootstrap';
import './Authorizations.css';
import { store } from 'react-notifications-component';

class Authorizations extends Component {
  constructor(props) {
    super(props);
    this.state = this.props.authData;
    this.state['isAuthorized'] = false;
    this.state['access_token'] = '';
    this.state['baseURL'] = this.getParamValue('iss');
    this.state['launch'] = this.getParamValue('launch');
    this.state['code'] = this.getParamValue('code');
    this.state['state'] = this.getParamValue('state');
    this.state['patientId'] = '';
    this.clientDetails = {};
    if (this.state.launch && this.state.baseURL) {
      this.getClientDetails(this.state.baseURL);
    }
    if (this.state.code && this.state.state) {
      this.getAuthorizeToken(this.state.code, this.state.state);
    }
  }

  geturl() {
    var protocol, context, host, strurl;
    protocol = window.location.protocol;
    host = window.location.host;
    // port = window.location.port;
    context = window.location.pathname.substring(0, window.location.pathname.indexOf("/", 2));
    strurl = protocol + "//" + host + context;
    return strurl;
  };

  getClientDetails(baseURL) {
    fetch(this.geturl() + "/api/clientDetails?url=" + baseURL)
      .then(function (response) {
        if (response.status === 200) {
          return response.json();
        } else {
          store.addNotification({
            title: '' + response.status + '',
            message: 'UnAuthorized. The Server is not registered.',
            type: 'danger',
            insert: 'bottom',
            container: 'bottom-right',
            animationIn: ['animated', 'fadeIn'],
            animationOut: ['animated', 'fadeOut'],
            dismiss: {
              duration: 5000,
              click: true,
              onScreen: true
            }
          });
        }
      }).then(result => {
        console.log(result);
        this.clientDetails = result;
        this.authorizeWithServer();
      });
  }

  getParamValue(variable) {
    var query = window.location.search.substring(1);
    var vars = query.split('&');
    for (var i = 0; i < vars.length; i++) {
      var pair = vars[i].split('=');
      if (pair[0] === variable) {
        return pair[1];
      }
    }
  }
  authorizeWithServer() {
    var state = Math.round(Math.random() * 100000000).toString();
    const clientid = this.clientDetails.clientId;
    const redirecturi = window.location.href.split('?')[0];
    const scopes = this.clientDetails.scopes;
    const strURL = decodeURIComponent(this.clientDetails.fhirServerBaseURL);
    var fhirVersion = '';
    if (strURL) {
      fetch(strURL + '/metadata?_format=json')
        .then(function (response) {
          return response.json();
        })
        .then(result => {
          if (typeof result == 'string') {
            result = JSON.parse(result);
          }
          if (result.fhirVersion === "1.0.2") {
            fhirVersion = 'DSTU2';
          }
          if (result.fhirVersion === "4.0.0") {
            fhirVersion = 'R4';
          }
          var arg = result.rest[0].security.extension[0].extension;
          for (var i = 0; i < arg.length; i++) {
            if (arg[i].url === 'register') {
              this.setState({
                regurl: arg[i].valueUri
              });
            } else if (arg[i].url === 'authorize') {
              this.setState({
                authurl: arg[i].valueUri
              });
            } else if (arg[i].url === 'token') {
              this.setState({
                tokenurl: arg[i].valueUri
              });
            }
          }
          sessionStorage.clear();
          sessionStorage[state] = JSON.stringify({
            clientid: clientid,
            authUrl: this.state.authurl,
            redirecturi: redirecturi,
            tokenurl: this.state.tokenurl,
            strurl: strURL,
            scopes: this.clientDetails.scopes,
            launch: this.state.launch,
            fhirVersion: fhirVersion,
            clientDetails: this.clientDetails
          });
          this.fhirOAuth(
            clientid,
            scopes,
            redirecturi,
            this.state.authurl,
            strURL,
            state
          );
        });
    }
  }

  fhirOAuth(clientId, scopes, redirectURI, authorizeURL, baseURL, state) {
    var path = authorizeURL + '?';
    var queryParams = [
      'response_type=code',
      'client_id=' + clientId,
      'redirect_uri=' + redirectURI,
      'launch=' + this.state.launch,
      'state=' + state,
      'scope=' + scopes,
      'aud=' + this.state.baseURL
    ];
    var query = queryParams.join('&');
    var url = path + query;
    console.log(url);
    window.location.replace(url);
  }

  getAuthorizeToken(code, state) {
    var params = JSON.parse(sessionStorage[state]);
    console.log(params.clientDetails);
    this.clientDetails = params.clientDetails;
    const tokenParams = {
      grant_type: 'authorization_code',
      code: code,
      redirect_uri: params.redirecturi,
      client_id: params.clientid
    };
    const searchParams = Object.keys(tokenParams)
      .map(key => {
        return (
          encodeURIComponent(key) + '=' + encodeURIComponent(tokenParams[key])
        );
      })
      .join('&');
    fetch(params.tokenurl, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded'
      },
      body: searchParams
    })
      .then(response => {
        return response.json();
      })
      .then(body => {
        console.log(body);
        this.setState({
          isAuthorized: true,
          access_token: body.access_token,
          refreshToken: body.refresh_token,
          expiry: body.expires_in,
          authUrl: params.authUrl,
          baseURL: params.strurl,
          patientId: body.patient,
          tokenUrl: params.tokenurl,
          userId: body.user,
          fhirVersion: params.fhirVersion
        });
        console.log(this.clientDetails);
        if (body.encounter) {
          this.setState({
            encounterId: body.encounter
          });
          this.getEncounterData();
        } else {
          this.setState({
            encounterId: "Unknown",
            encounterStartDate: this.getStartDate(Number(this.clientDetails.encounterStartThreshold)),
            encounterEndDate: this.getEndDate(Number(this.clientDetails.encounterEndThreshold))
          });
          this.submitClientDetails();
        }
        console.log(this.state);
        this.getResourcesData();
        // setTimeout(() => {

        // }, 3000);
      });
  }

  submitClientDetails() {
    console.log(this.state);
    const clientInfo = {
      clientId: this.clientDetails.clientId,
      ehrServerURL: this.state.baseURL,
      authUrl: this.state.authUrl,
      tokenUrl: this.state.tokenUrl,
      accessToken: this.state.access_token,
      refreshToken: this.state.refreshToken,
      userId: this.state.userId,
      expiry: this.state.expiry,
      scope: this.clientDetails.scopes,
      launchPatientId: this.state.patientId,
      fhirVersion: this.state.fhirVersion,
      encounterId: this.state.encounterId,
      assigningAuthorityId: this.clientDetails.assigningAuthorityId,
      setId: this.state.patientId + "+" + this.state.encounterId,
      versionNumber: "1",
      directUser: this.clientDetails.directUser,
      directPwd: this.clientDetails.directPwd,
      startDate: Date.parse(this.state.encounterStartDate),
      endDate: Date.parse(this.state.encounterEndDate),
      directHost: this.clientDetails.directHost,
      directRecipient: this.clientDetails.directRecipientAddress
    };
    console.log(clientInfo);

    fetch(this.geturl() + "/api/launchDetails", {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(clientInfo)
    })
      .then(response => {
        if (response.status === 200) {
          return response.json();
        } else {
          store.addNotification({
            title: '' + response.status + '',
            message: 'Error in Saving Launch Details.',
            type: 'danger',
            insert: 'bottom',
            container: 'bottom-right',
            animationIn: ['animated', 'fadeIn'],
            animationOut: ['animated', 'fadeOut'],
            dismiss: {
              duration: 5000,
              click: true,
              onScreen: true
            }
          });
        }
      })
      .then(body => {
        console.log(body);
      });
  }

  getResourcesData() {
    const patientData = this.getPatientData();
    console.log(patientData);
  }
  getPatientData() {
    fetch(
      this.state.baseURL + '/Patient/' + this.state.patientId + '?_format=json',
      {
        method: 'GET',
        headers: {
          Authorization: 'Bearer ' + this.state.access_token
        }
      }
    )
      .then(response => {
        if (response.status === 200) {
          return response.json();
        } else {
          store.addNotification({
            title: '' + response.status + '',
            message: 'Error in Getting Patient Details.',
            type: 'danger',
            insert: 'bottom',
            container: 'bottom-right',
            animationIn: ['animated', 'fadeIn'],
            animationOut: ['animated', 'fadeOut'],
            dismiss: {
              duration: 5000,
              click: true,
              onScreen: true
            }
          });
          return;
        }

      })
      .then(result => {
        console.log("Received Patient Data==============>" + JSON.stringify(result));
      });
  }

  getEncounterData() {
    fetch(
      this.state.baseURL + '/Encounter/' + this.state.encounterId + '?_format=json',
      {
        method: 'GET',
        headers: {
          Authorization: 'Bearer ' + this.state.access_token
        }
      }
    )
      .then(response => {
        if (response.status === 200) {
          return response.json();
        } else {
          store.addNotification({
            title: '' + response.status + '',
            message: 'Error in Getting Encounter Details.',
            type: 'danger',
            insert: 'bottom',
            container: 'bottom-right',
            animationIn: ['animated', 'fadeIn'],
            animationOut: ['animated', 'fadeOut'],
            dismiss: {
              duration: 5000,
              click: true,
              onScreen: true
            }
          });
          return;
        }

      })
      .then(result => {
        const encounter = result;
        if (encounter.period) {
          const period = encounter.period;
          if (period.start) {
            this.setState({
              encounterStartDate: period.start
            });
          } else {
            this.setState({
              encounterStartDate: this.getStartDate(Number(this.clientDetails.encounterStartThreshold))
            });
          }

          if (period.end) {
            this.setState({
              encounterEndDate: period.end
            });
          } else {
            this.setState({
              encounterEndDate: this.getEndDate(Number(this.clientDetails.encounterEndThreshold))
            });
          }
        }
        this.submitClientDetails();
      });
  }

  getStartDate(startThreshold) {
    const startDate = new Date();
    startDate.setHours(startDate.getHours() - startThreshold);
    const year = startDate.getFullYear();
    const currentMonth = startDate.getMonth() + 1;
    const month = startDate.getMonth() < 10 ? "0" + currentMonth : startDate.getMonth();
    const date = startDate.getDate() < 10 ? "0" + startDate.getDate() : startDate.getDate();
    const hours = startDate.getHours() < 10 ? "0" + startDate.getHours() : startDate.getHours();
    const minutes = startDate.getMinutes() < 10 ? "0" + startDate.getMinutes() : startDate.getMinutes();
    const seconds = startDate.getSeconds() < 10 ? "0" + startDate.getSeconds() : startDate.getSeconds();
    console.log(year + "-" + month + "-" + date + "T" + hours + ":" + minutes + ":" + seconds + ".000Z");
    return year + "-" + month + "-" + date + "T" + hours + ":" + minutes + ":" + seconds + ".000Z";
  }

  getEndDate(endThreshold) {
    const endDate = new Date();
    endDate.setHours(endDate.getHours() + endThreshold);
    const year = endDate.getFullYear();
    const currentMonth = endDate.getMonth() + 1;
    const month = endDate.getMonth() < 10 ? "0" + currentMonth : endDate.getMonth();
    const date = endDate.getDate() < 10 ? "0" + endDate.getDate() : endDate.getDate();
    const hours = endDate.getHours() < 10 ? "0" + endDate.getHours() : endDate.getHours();
    const minutes = endDate.getMinutes() < 10 ? "0" + endDate.getMinutes() : endDate.getMinutes();
    const seconds = endDate.getSeconds() < 10 ? "0" + endDate.getSeconds() : endDate.getSeconds();
    console.log(year + "-" + month + "-" + date + "T" + hours + ":" + minutes + ":" + seconds + ".000Z");
    return year + "-" + month + "-" + date + "T" + hours + ":" + minutes + ":" + seconds + ".000Z";
  }

  render() {
    const setShow = () => this.setState({ isAuthorized: false });
    return (
      <div className="authorizations">
        <Alert
          variant="success"
          show={this.state.isAuthorized}
          onClose={() => setShow(false)}
          dismissible
        >
          Application has been authorized with EHR successfully.
        </Alert>
        <div className="requests text-center">
          <Row>
            <Col>
              <p>This "splash screen" will only be present if the eCR Now App has not been automatically launched. It is preferable to launch the App with an automated process at the start of encounters that does not have this user interface, does not impact healthcare provider workflow, and insures consistent operation. Specific launch implementation will depend on the EHR. The launch API is detailed in the available implementation specifications.</p>
              <p>In the circumstance that automated launching cannot be accomplished, this text can be replaced with an appropriate message.</p>
            </Col>
          </Row>
        </div>
        <div className="centeredDiv text-center">
          <button
            className="btn btn-primary submitBtn"
            type="button"
          // onClick={() => {
          //   store.addNotification({
          //     title: 'Success',
          //     message: 'Client Details are saved successfully.',
          //     type: 'success',
          //     insert: 'bottom',
          //     container: 'bottom-right',
          //     animationIn: ['animated', 'fadeIn'],
          //     animationOut: ['animated', 'fadeOut'],
          //     dismiss: {
          //       duration: 5000,
          //       click: true,
          //       onScreen: true
          //     }
          //   });
          // }}
          >
            Ok
                        </button>
        </div>
      </div >
    );
  }
}

export default Authorizations;

import React, { Component } from 'react';
import {
    Alert,
    Row,
    Col,
    Form, Card, Accordion
} from 'react-bootstrap';
import './ClientDetails.css';
import { store } from 'react-notifications-component';

class ClientDetails extends Component {
    constructor(props) {
        super(props);
        this.state = {};
        this.state.launchType = 'providerLaunch';
        this.state.directType = 'direct';
        this.state.reportType = 'covid19';
        this.state.isSaved = false;
        this.saveClientDetails = this.saveClientDetails.bind(this);
        this.handleRadioChange = this.handleRadioChange.bind(this);
        this.handleDirectChange = this.handleDirectChange.bind(this);
        this.handleReportChange = this.handleReportChange.bind(this);
    }

    handleChange(e) {
        this.setState({
            [e.target.name]: e.target.value
        });
    }

    handleRadioChange(e) {
        this.setState({
            launchType: e.target.value
        });
    }

    handleDirectChange(e) {
        this.setState({
            directType: e.target.value
        });
    }
    handleReportChange(e) {
        this.setState({
            reportType: e.target.value
        });
    }

    geturl() {
        var protocol, context, host, strurl;
        protocol = window.location.protocol;
        host = window.location.host;
        //port = window.location.port;
        context = window.location.pathname.substring(0, window.location.pathname.indexOf("/", 2));
        strurl = protocol + "//" + host + context;
        return strurl;
    };

    saveClientDetails() {
        console.log("clicked");
        var clientDetails = {
            isProvider: this.state.launchType === "providerLaunch" ? true : false,
            isSystem: this.state.launchType === 'systemLaunch' ? true : false,
            clientId: this.state.clientId,
            clientSecret: this.state.clientSecret ? this.state.clientSecret : null,
            fhirServerBaseURL: this.state.fhirServerBaseURL,
            tokenURL: this.state.tokenEndpoint ? this.state.tokenEndpoint : null,
            scopes: this.state.scopes,
            isDirect: this.state.directType === "direct" ? true : false,
            isXdr: this.state.directType === "xdr" ? true : false,
            directHost: this.state.directHost,
            directUser: this.state.directUserName,
            directPwd: this.state.directPwd,
            directRecipientAddress: this.state.directRecipientAddress,
            xdrRecipientAddress: this.state.xdrRecipientAddress,
            assigningAuthorityId: this.state.assigningAuthorityId,
            encounterStartThreshold: this.state.startThreshold,
            encounterEndThreshold: this.state.endThreshold,
            isCovid: this.state.reportType === "covid19" ? true : false,
            isFullEcr: this.state.reportType === "fullecr" ? true : false,
            ersdFileLocation: this.state.ersdFileLocation,
            schematronLocation: this.state.schematronLocation
        };
        console.log(this.geturl());
        console.log(JSON.stringify(clientDetails));
        var serviceURL = this.geturl();
        fetch(serviceURL + "/api/clientDetails", {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(clientDetails)
        })
            .then(response => {
                if (response.status === 200) {
                    this.setState({
                        isSaved: true
                    });
                    return response.json();
                } else {
                    store.addNotification({
                        title: '' + response.status + '',
                        message: 'Error in Saving the Client Details',
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
                console.log(result);
                if (result) {
                    this.setState({
                        launchType: "providerLaunch",
                        clientId: "",
                        clientSecret: "",
                        fhirServerBaseURL: "",
                        tokenEndpoint: "",
                        scopes: "",
                        directType: "direct",
                        directHost: "",
                        directUserName: "",
                        directPwd: "",
                        directRecipientAddress: "",
                        xdrRecipientAddress: "",
                        assigningAuthorityId: "",
                        startThreshold: "",
                        endThreshold: "",
                        reportType: "covid19",
                        ersdFileLocation: "",
                        schematronLocation: ""
                    });
                    store.addNotification({
                        title: 'Success',
                        message: 'Client Details are saved successfully.',
                        type: 'success',
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

            });
    }

    render() {
        const setShow = () => this.setState({ isSaved: false });
        return (
            <div className="clientDetails">
                <br />
                <Row>
                    <Col>
                        <h2>Client Details</h2>
                    </Col>
                    <Col>
                        <Row>
                            <Col>
                            </Col>
                            <Col>
                            </Col>
                        </Row>
                    </Col>
                </Row>
                <hr />
                <Row>
                    <Col>
                        <Alert
                            variant="success"
                            show={this.state.isSaved}
                            onClose={() => setShow(false)}
                            dismissible
                        >
                            Client Details are saved successfully.
        </Alert>
                        <Form ref={form => this.messageForm = form}>
                            <Accordion defaultActiveKey="0">
                                <Card className="accordionCards">
                                    <Accordion.Toggle as={Card.Header} eventKey="0">
                                        FHIR Configuration
                                    </Accordion.Toggle>
                                    <Accordion.Collapse eventKey="0">
                                        <Card.Body className="fhirConfiguration">
                                            <Form.Group as={Row} controlId="formHorizontalClientId">
                                                <Form.Label column sm={2}>
                                                    Launch Type:
                                    </Form.Label>
                                                <Col sm={10}>
                                                    <Row>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="providerLaunch">
                                                                <Form.Check.Input type="radio" checked={this.state.launchType === 'providerLaunch'} value="providerLaunch" name="launchType" onChange={e => this.handleRadioChange(e)} />
                                                                <Form.Check.Label>Provider Launch</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="systemLaunch">
                                                                <Form.Check.Input type="radio" checked={this.state.launchType === 'systemLaunch'} value="systemLaunch" onChange={e => this.handleRadioChange(e)} />
                                                                <Form.Check.Label>System Launch</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                    </Row>
                                                </Col>
                                            </Form.Group>
                                            <Form.Group as={Row} controlId="formHorizontalClientId">
                                                <Form.Label column sm={2}>
                                                    Client Id:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="ClientId" name="clientId" onChange={e => this.handleChange(e)} value={this.state.clientId} />
                                                </Col>
                                            </Form.Group>

                                            {this.state.launchType === 'systemLaunch' ? (
                                                <Form.Group as={Row} controlId="formHorizontalClientSecret">
                                                    <Form.Label column sm={2}>
                                                        Client Secret:
                                                    </Form.Label>
                                                    <Col sm={10}>
                                                        <Form.Control type="text" placeholder="Client Secret" name="clientSecret" onChange={e => this.handleChange(e)} value={this.state.clientSecret} />
                                                    </Col>
                                                </Form.Group>
                                            ) : ''}

                                            <Form.Group as={Row} controlId="formHorizontalScopes">
                                                <Form.Label column sm={2}>
                                                    Scopes:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control as="textarea" rows="3" name="scopes" onChange={e => this.handleChange(e)} value={this.state.scopes} />
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="formHorizontalFHIRBaseURL">
                                                <Form.Label column sm={2}>
                                                    FHIR Server Base URL:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="FHIR Server Base URL" name="fhirServerBaseURL" onChange={e => this.handleChange(e)} value={this.state.fhirServerBaseURL} />
                                                </Col>
                                            </Form.Group>

                                            {this.state.launchType === 'systemLaunch' ? (
                                                <Form.Group as={Row} controlId="formHorizontalTokenURL">
                                                    <Form.Label column sm={2}>
                                                        Token Endpoint:
                                                    </Form.Label>
                                                    <Col sm={10}>
                                                        <Form.Control type="text" placeholder="Token Endpoint" name="tokenEndpoint" onChange={e => this.handleChange(e)} value={this.state.tokenEndpoint} />
                                                    </Col>
                                                </Form.Group>
                                            ) : ''}
                                        </Card.Body>
                                    </Accordion.Collapse>
                                </Card>

                                <Card className="accordionCards">
                                    <Accordion.Toggle as={Card.Header} eventKey="1">
                                        Transport Configuration
                                    </Accordion.Toggle>
                                    <Accordion.Collapse eventKey="1">
                                        <Card.Body className="transportConfiguration">
                                            <Form.Group as={Row} controlId="formHorizontalClientId">
                                                <Form.Label column sm={2}>
                                                    Direct Type:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Row>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="direct">
                                                                <Form.Check.Input type="radio" checked={this.state.directType === 'direct'} value="direct" onChange={e => this.handleDirectChange(e)} />
                                                                <Form.Check.Label>Direct</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="xdr">
                                                                <Form.Check.Input type="radio" checked={this.state.directType === 'xdr'} value="xdr" onChange={e => this.handleDirectChange(e)} />
                                                                <Form.Check.Label>XDR</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                    </Row>
                                                </Col>
                                            </Form.Group>
                                            {this.state.directType === 'direct' ? (
                                                <div>
                                                    <Form.Group as={Row} controlId="directHost">
                                                        <Form.Label column sm={2}>
                                                            Direct Host:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="text" placeholder="Direct Host" name="directHost" onChange={e => this.handleChange(e)} value={this.state.directHost} />
                                                        </Col>
                                                    </Form.Group>


                                                    <Form.Group as={Row} controlId="directUserName">
                                                        <Form.Label column sm={2}>
                                                            Direct Sender User Name:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="email" placeholder="Direct Sender User Name" name="directUserName" onChange={e => this.handleChange(e)} value={this.state.directUserName} />
                                                        </Col>
                                                    </Form.Group>

                                                    <Form.Group as={Row} controlId="directPwd">
                                                        <Form.Label column sm={2}>
                                                            Direct Sender Password:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="password" name="directPwd" placeholder="Direct Sender Password" onChange={e => this.handleChange(e)} value={this.state.directPwd} />
                                                        </Col>
                                                    </Form.Group>

                                                    <Form.Group as={Row} controlId="directRecipientAddress">
                                                        <Form.Label column sm={2}>
                                                            Direct Recipient Address:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="email" name="directRecipientAddress" placeholder="Direct Receipient Address" onChange={e => this.handleChange(e)} value={this.state.directRecipientAddress} />
                                                        </Col>
                                                    </Form.Group>
                                                </div>
                                            ) : ''}

                                            {this.state.directType === 'xdr' ? (
                                                <div>
                                                    <Form.Group as={Row} controlId="xdrRecipientAddress">
                                                        <Form.Label column sm={2}>
                                                            XDR Recipient Address:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="text" placeholder="XDR Recipient Address" name="xdrRecipientAddress" onChange={e => this.handleChange(e)} value={this.state.xdrRecipientAddress} />
                                                        </Col>
                                                    </Form.Group>
                                                </div>
                                            ) : ''}
                                        </Card.Body>
                                    </Accordion.Collapse>
                                </Card>

                                <Card className="accordionCards">
                                    <Accordion.Toggle as={Card.Header} eventKey="2">
                                        App Configuration
                                    </Accordion.Toggle>
                                    <Accordion.Collapse eventKey="2">
                                        <Card.Body className="appConfiguration">
                                            <Form.Group as={Row} controlId="assigningAuthorityId">
                                                <Form.Label column sm={2}>
                                                    Assigning Authority Id:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Assigning Authority Id" name="assigningAuthorityId" onChange={e => this.handleChange(e)} value={this.state.assigningAuthorityId} />
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="startThreshold">
                                                <Form.Label column sm={2}>
                                                    Encounter Start Time Threshold:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Encounter Start Time Threshold" name="startThreshold" onChange={e => this.handleChange(e)} value={this.state.startThreshold} />
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="endThreshold">
                                                <Form.Label column sm={2}>
                                                    Encounter End Time Threshold:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Encounter End Time Threshold" name="endThreshold" onChange={e => this.handleChange(e)} value={this.state.endThreshold} />
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="reportType">
                                                <Form.Label column sm={2}>
                                                    Report Type:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Row>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="covid19">
                                                                <Form.Check.Input type="radio" checked={this.state.reportType === 'covid19'} value="covid19" onChange={e => this.handleReportChange(e)} />
                                                                <Form.Check.Label>Covid-19</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                        <Col sm={4}>
                                                            <Form.Check type="radio" id="fullecr">
                                                                <Form.Check.Input type="radio" checked={this.state.reportType === 'fullecr'} value="fullecr" onChange={e => this.handleReportChange(e)} />
                                                                <Form.Check.Label>Full ECR Report</Form.Check.Label>
                                                            </Form.Check>
                                                        </Col>
                                                    </Row>
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="ersdFile">
                                                <Form.Label column sm={2}>
                                                    ERSD File Location:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="ERSD File Location" name="ersdFileLocation" onChange={e => this.handleChange(e)} value={this.state.ersdFileLocation} />
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="schematronLocation">
                                                <Form.Label column sm={2}>
                                                    Schematron Location:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Schematron Location" name="schematronLocation" onChange={e => this.handleChange(e)} value={this.state.schematronLocation} />
                                                </Col>
                                            </Form.Group>
                                        </Card.Body>
                                    </Accordion.Collapse>
                                </Card>
                            </Accordion>
                        </Form>
                        <Row>
                            <Col className="text-center">
                                <button
                                    className="btn btn-primary submitBtn"
                                    type="button"
                                    onClick={e => this.saveClientDetails(e)}
                                >
                                    Save
                                </button>
                            </Col>
                        </Row>
                    </Col>
                </Row>
            </div>
        );
    }
}

export default ClientDetails;

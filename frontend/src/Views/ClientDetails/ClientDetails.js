import React, { Component } from 'react';
import {
    Alert,
    Row,
    Col,
    Form, Card, Accordion, Button
} from 'react-bootstrap';
import './ClientDetails.css';
import { store } from 'react-notifications-component';

class ClientDetails extends Component {
    constructor(props) {
        super(props);
        this.state = {
            validated: false
        };
        this.selectedClientDetails = this.props.selectedClientDetails;
        this.addNew = this.props.addNew ? this.props.addNew.addNew : false;
        if (!this.addNew && !this.isEmpty(this.selectedClientDetails)) {
            if (this.selectedClientDetails.isProvider) {
                this.state.launchType = 'providerLaunch';
            }
            if (this.selectedClientDetails.isSystem) {
                this.state.launchType = 'systemLaunch';
            }
            this.state.clientId = this.selectedClientDetails.clientId;
            this.state.clientSecret = this.selectedClientDetails.clientSecret;
            this.state.fhirServerBaseURL = this.selectedClientDetails.fhirServerBaseURL;
            this.state.tokenEndpoint = this.selectedClientDetails.tokenURL;
            this.state.scopes = this.selectedClientDetails.scopes;
            if (this.selectedClientDetails.isDirect) {
                this.state.directType = 'direct';
            }
            if (this.selectedClientDetails.isXdr) {
                this.state.directType = 'xdr';
            }
            this.state.directHost = this.selectedClientDetails.directHost;
            this.state.directUserName = this.selectedClientDetails.directUser;
            this.state.directPwd = this.selectedClientDetails.directPwd;
            this.state.directRecipientAddress = this.selectedClientDetails.directRecipientAddress;
            this.state.xdrRecipientAddress = this.selectedClientDetails.xdrRecipientAddress;
            this.state.assigningAuthorityId = this.selectedClientDetails.assigningAuthorityId;
            this.state.startThreshold = this.selectedClientDetails.encounterStartThreshold;
            this.state.endThreshold = this.selectedClientDetails.encounterEndThreshold;
            if (this.selectedClientDetails.isCovid) {
                this.state.reportType = "covid19";
            }
            if (this.selectedClientDetails.isFullEcr) {
                this.state.reportType = "fullecr";
            }
        } else {
            this.state.launchType = 'providerLaunch';
            this.state.directType = 'direct';
            this.state.reportType = 'covid19';
        }
        this.state.isSaved = false;
        this.saveClientDetails = this.saveClientDetails.bind(this);
        this.handleRadioChange = this.handleRadioChange.bind(this);
        this.handleDirectChange = this.handleDirectChange.bind(this);
        this.handleReportChange = this.handleReportChange.bind(this);
        this.openClientDetailsList = this.openClientDetailsList.bind(this);
    }

    isEmpty(obj) {
        for (var key in obj) {
            if (obj.hasOwnProperty(key))
                return false;
        }
        return true;
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

    openClientDetailsList() {
        this.props.history.push('clientDetailsList');
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
        var requestMethod = '';
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
            isFullEcr: this.state.reportType === "fullecr" ? true : false
        };
        if (!this.addNew && this.selectedClientDetails) {
            clientDetails['id'] = this.selectedClientDetails.id;
            requestMethod = 'PUT';
        } else {
            requestMethod = 'POST';
        }
        console.log(this.geturl());
        console.log(JSON.stringify(clientDetails));
        var serviceURL = this.geturl();
        fetch(serviceURL + "/api/clientDetails", {
            method: requestMethod,
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
                    const errorMessage = response.json();
                    console.log(errorMessage);
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

                    this.openClientDetailsList();
                }

            });
    }

    render() {
        const setShow = () => this.setState({ isSaved: false });

        const handleSubmit = (event) => {
            const form = event.currentTarget;
            if (form.checkValidity() === false) {
                event.preventDefault();
                event.stopPropagation();
            }
            if (form.checkValidity() === true) {
                this.saveClientDetails();
                this.setState({
                    validated: true
                });
                event.preventDefault();
                event.stopPropagation();
            }

        };
        return (
            <div className="clientDetails">
                <br />
                <Row>
                    <Col md="6">
                        <h2>Client Details Configuration</h2>
                    </Col>
                    <Col md="6" className="clientCol">
                        <Button onClick={this.openClientDetailsList}>Existing Client Details</Button>
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
                        <Form noValidate validated={this.state.validated} onSubmit={handleSubmit} >
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
                                                    <Form.Control type="text" placeholder="ClientId" name="clientId" required onChange={e => this.handleChange(e)} value={this.state.clientId} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide a Client Id.
                                                    </Form.Control.Feedback>
                                                </Col>
                                            </Form.Group>

                                            {this.state.launchType === 'systemLaunch' ? (
                                                <Form.Group as={Row} controlId="formHorizontalClientSecret">
                                                    <Form.Label column sm={2}>
                                                        Client Secret:
                                                    </Form.Label>
                                                    <Col sm={10}>
                                                        <Form.Control type="text" placeholder="Client Secret" name="clientSecret" required={this.state.launchType === 'systemLaunch' ? true : false} onChange={e => this.handleChange(e)} value={this.state.clientSecret} />
                                                        <Form.Control.Feedback type="invalid">
                                                            Please provide a Client Secret.
                                                        </Form.Control.Feedback>
                                                    </Col>
                                                </Form.Group>
                                            ) : ''}

                                            <Form.Group as={Row} controlId="formHorizontalScopes">
                                                <Form.Label column sm={2}>
                                                    Scopes:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control as="textarea" rows="3" name="scopes" onChange={e => this.handleChange(e)} required value={this.state.scopes} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide Scopes.
                                                    </Form.Control.Feedback>
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="formHorizontalFHIRBaseURL">
                                                <Form.Label column sm={2}>
                                                    FHIR Server Base URL:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="FHIR Server Base URL" name="fhirServerBaseURL" required onChange={e => this.handleChange(e)} value={this.state.fhirServerBaseURL} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide a FHIR Server Base URL.
                                                    </Form.Control.Feedback>
                                                </Col>
                                            </Form.Group>

                                            {this.state.launchType === 'systemLaunch' ? (
                                                <Form.Group as={Row} controlId="formHorizontalTokenURL">
                                                    <Form.Label column sm={2}>
                                                        Token Endpoint:
                                                    </Form.Label>
                                                    <Col sm={10}>
                                                        <Form.Control type="text" placeholder="Token Endpoint" name="tokenEndpoint" required={this.state.launchType === 'systemLaunch' ? true : false} onChange={e => this.handleChange(e)} value={this.state.tokenEndpoint} />

                                                        <Form.Control.Feedback type="invalid">
                                                            Please provide a FHIR Server Token URL.
                                                        </Form.Control.Feedback>
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
                                                            <Form.Control type="text" placeholder="Direct Host" name="directHost" required={this.state.directType === 'direct' ? true : false} onChange={e => this.handleChange(e)} value={this.state.directHost} />
                                                            <Form.Control.Feedback type="invalid">
                                                                Please provide a Direct Host name.
                                                            </Form.Control.Feedback>
                                                        </Col>
                                                    </Form.Group>


                                                    <Form.Group as={Row} controlId="directUserName">
                                                        <Form.Label column sm={2}>
                                                            Direct Sender User Name:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="text" placeholder="Direct Sender User Name" required={this.state.directType === 'direct' ? true : false} name="directUserName" onChange={e => this.handleChange(e)} value={this.state.directUserName} />
                                                            <Form.Control.Feedback type="invalid">
                                                                Please provide a Direct Sender User Name.
                                                            </Form.Control.Feedback>
                                                        </Col>
                                                    </Form.Group>

                                                    <Form.Group as={Row} controlId="directPwd">
                                                        <Form.Label column sm={2}>
                                                            Direct Sender Password:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="password" name="directPwd" placeholder="Direct Sender Password" required={this.state.directType === 'direct' ? true : false} onChange={e => this.handleChange(e)} value={this.state.directPwd} />
                                                            <Form.Control.Feedback type="invalid">
                                                                Please provide a Direct Password.
                                                            </Form.Control.Feedback>
                                                        </Col>
                                                    </Form.Group>

                                                    <Form.Group as={Row} controlId="directRecipientAddress">
                                                        <Form.Label column sm={2}>
                                                            Direct Recipient Address:
                                                        </Form.Label>
                                                        <Col sm={10}>
                                                            <Form.Control type="text" name="directRecipientAddress" required={this.state.directType === 'direct' ? true : false} placeholder="Direct Receipient Address" onChange={e => this.handleChange(e)} value={this.state.directRecipientAddress} />
                                                            <Form.Control.Feedback type="invalid">
                                                                Please provide a Direct Recipient Address.
                                                            </Form.Control.Feedback>
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
                                                            <Form.Control type="text" placeholder="XDR Recipient Address" required={this.state.directType === 'xdr' ? true : false} name="xdrRecipientAddress" onChange={e => this.handleChange(e)} value={this.state.xdrRecipientAddress} />
                                                            <Form.Control.Feedback type="invalid">
                                                                Please provide a XDR Recipient Address.
                                                            </Form.Control.Feedback>
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
                                                    <Form.Control type="text" placeholder="Assigning Authority Id" required name="assigningAuthorityId" onChange={e => this.handleChange(e)} value={this.state.assigningAuthorityId} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide a Assigning Authority Id.
                                                    </Form.Control.Feedback>
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="startThreshold">
                                                <Form.Label column sm={2}>
                                                    Encounter Start Time Threshold:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Encounter Start Time Threshold" required name="startThreshold" onChange={e => this.handleChange(e)} value={this.state.startThreshold} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide a Encounter Start Time Threshold.
                                                    </Form.Control.Feedback>
                                                </Col>
                                            </Form.Group>

                                            <Form.Group as={Row} controlId="endThreshold">
                                                <Form.Label column sm={2}>
                                                    Encounter End Time Threshold:
                                                </Form.Label>
                                                <Col sm={10}>
                                                    <Form.Control type="text" placeholder="Encounter End Time Threshold" required name="endThreshold" onChange={e => this.handleChange(e)} value={this.state.endThreshold} />
                                                    <Form.Control.Feedback type="invalid">
                                                        Please provide a Encounter End Time Threshold.
                                                    </Form.Control.Feedback>
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
                                        </Card.Body>
                                    </Accordion.Collapse>
                                </Card>
                            </Accordion>
                            <Row>
                                <Col className="text-center">
                                    <Button type="submit">Save</Button>
                                </Col>
                            </Row>
                        </Form>
                        {/* <Row>
                            <Col className="text-center">
                                <button
                                    className="btn btn-primary submitBtn"
                                    type="button"
                                    onClick={e => this.saveClientDetails(e)}
                                >
                                    Save
                                </button>
                            </Col>
                        </Row> */}
                    </Col>
                </Row>
            </div>
        );
    }
}

export default ClientDetails;

import React, { Component } from 'react';
import {
    Row,
    Col, Button, Table, OverlayTrigger, Tooltip
} from 'react-bootstrap';
import './ClientDetailsList.css';
import { store } from 'react-notifications-component';
import Icon from '@material-ui/core/Icon';

const tooltip = <Tooltip id="tooltip">Edit</Tooltip>;

class ClientDetailsList extends Component {
    constructor(props) {
        super(props);
        this.state = {
            details: []
        };

        this.openAddNewClient = this.openAddNewClient.bind(this);
    }

    componentDidMount() {
        this.getAllClientDetails();
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

    getAllClientDetails() {
        const serviceURL = this.geturl();
        fetch(serviceURL + "/api/clientDetails/", {
            method: 'GET'
        }).then(response => {
            console.log(response);
            if (response.status !== 200) {
                store.addNotification({
                    title: '' + response.status + '',
                    message: 'Error in getting the Client Details',
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
            } else {
                return response.json();
            }
        }).then(result => {
            console.log(result);
            this.setState({
                details: result
            });
        });
    }

    openAddNewClient() {
        this.props.addNew({ addNew: true });
        this.props.history.push('clientDetails');
    }

    editClient(selectedClientDetails) {
        console.log(selectedClientDetails);
        this.props.addNew({ addNew: false });
        this.props.selectedClientDetails(selectedClientDetails);
        this.props.history.push('clientDetails');
    }


    render() {
        return (
            <div className="clientDetails">
                <br />
                <Row>
                    <Col md="6">
                        <h2>Client Details List</h2>
                    </Col>
                    <Col className="addClient">
                        <Button onClick={this.openAddNewClient}>Add Client Details</Button>
                    </Col>
                </Row>
                <hr />
                <Row>
                    <Col>
                        <Table responsive="lg" striped bordered hover size="sm">
                            <tbody>
                                <tr>
                                    <th>Id</th>
                                    <th>Client Id</th>
                                    <th>FHIR Server Url</th>
                                    <th>Direct Host</th>
                                    <th>Action</th>
                                </tr>
                                {
                                    this.state.details.map(get =>
                                        <tr key={get.id}>
                                            <td>{get.id}</td>
                                            <td>{get.clientId}</td>
                                            <td>{get.fhirServerBaseURL}</td>
                                            <td>{get.directHost}</td>
                                            <td className="actionColumn">
                                                <OverlayTrigger placement="top" overlay={tooltip}>
                                                    <Button className="editButton" onClick={e => this.editClient(get)}><Icon>edit</Icon></Button></OverlayTrigger></td>
                                        </tr>
                                    )
                                }
                            </tbody>
                        </Table>
                    </Col>
                </Row>
            </div>
        );
    }
}

export default ClientDetailsList;

import React, { Component } from 'react';
import './App.css';
import { Container } from 'react-bootstrap';
import Header from './Layout/Header/Header';
import Authorizations from './Views/Authorizations/Authorizations';
import ClientDetails from './Views/ClientDetails/ClientDetails';
import ClientDetailsList from './Views/ClientDetailsList/ClientDetailsList';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import ReactNotification from 'react-notifications-component';

class App extends Component {
  constructor() {
    super();
    this.state = {
      displayJSONObject: false,
      isAuthorized: false,
      selectedClientDetails: {}
    };
    this.selectedClientDetails = this.selectedClientDetails.bind(this);
    this.addNew = this.addNew.bind(this);
  }

  async selectedClientDetails(_state) {
    const updatedState = JSON.parse(JSON.stringify(_state));
    await this.setState({
      selectedClientDetails: updatedState
    });
  }

  async addNew(_state) {
    const updatedState = JSON.parse(JSON.stringify(_state));
    await this.setState({
      addNew: updatedState
    });
  }

  render() {
    return (
      <div className="App">
        <Header />
        <div className="main">
          <Container>
            <Router>
              <Switch>
                <Route exact path="/" render={props => (<Authorizations {...props} authData={this.state}></Authorizations>)}></Route>
              </Switch>
              <Switch>
                <Route exact path="/clientDetails" render={props => (<ClientDetails {...props} selectedClientDetails={this.state.selectedClientDetails} addNew={this.state.addNew}></ClientDetails>)}></Route>
              </Switch>
              <Switch>
                <Route exact path="/clientDetailsList" render={props => (<ClientDetailsList {...props} selectedClientDetails={this.selectedClientDetails} addNew={this.addNew}></ClientDetailsList>)}></Route>
              </Switch>
            </Router>
          </Container>
        </div>
        <ReactNotification />
      </div>
    );
  }
}

export default App;

import React, { Component } from 'react';
import './App.css';
import { Container } from 'react-bootstrap';
import Header from './Layout/Header/Header';
import Authorizations from './Views/Authorizations/Authorizations';
import ClientDetails from './Views/ClientDetails/ClientDetails';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import ReactNotification from 'react-notifications-component';

class App extends Component {
  constructor() {
    super();
    this.state = {
      displayJSONObject: false,
      isAuthorized: false
    };
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
                <Route exact path="/clientDetails" render={props => (<ClientDetails {...props}></ClientDetails>)}></Route>
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

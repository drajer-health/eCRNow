import React, { Component } from 'react';
import './App.css';
import { Container } from 'react-bootstrap';
import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
import Header from './Layout/Header/Header';
import Authorizations from './Views/Authorizations/Authorizations';
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
        {/* <ClientDetails /> */}
        <div className="main">
          <Container>
            <Router basename={'/plandefinition-processor'}>
              {/* <Router> */}
              <Switch>
                <Route
                  exact
                  path="/"
                  // component={Authorizations}
                  render={props => (
                    <Authorizations
                      {...props}
                      authData={this.state}
                    />
                  )}
                />
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

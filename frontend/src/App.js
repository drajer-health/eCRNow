import React, { Component } from 'react';
import './App.css';
import { Container } from 'react-bootstrap';
import Header from './Layout/Header/Header';
import Authorizations from './Views/Authorizations/Authorizations';

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
            <Authorizations
              authData={this.state}
            />
          </Container>
        </div>
      </div>
    );
  }
}

export default App;

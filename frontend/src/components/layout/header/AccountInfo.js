import React from 'react';
import styled from 'styled-components/macro';
import { reduceBalance } from '../../../utils/reduceBalance';
import reduceToken from '../../../utils/reduceToken';
import "../../../styles/inputoverride.css"

const Container = styled.div`
  display: flex;
  border-color: 'pink';
  :hover {
    opacity: 0.7;
    cursor: pointer;
  }
`;

const CoinInfo = ({ account, balance, onClick }) => {
  return (
    <Container onClick={onClick}>
      <div className="balance-container">{balance}</div>
      <div className="account-container">{account}</div>
    </Container>
  );
};

export default CoinInfo;

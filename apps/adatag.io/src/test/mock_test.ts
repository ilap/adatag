import React from "react";
import { render, fireEvent, waitFor } from "@testing-library/react";
import MintDetailsDialog from "./MintDetailsDialog";
import { checkAvailability, calculateDeposit, constructBaseTx, constructTree, constructTxToComplete, signAndSubmitTx } from "your-cardano-library"; // replace with your Cardano library

jest.mock("your-cardano-library", () => ({
  checkAvailability: jest.fn(),
  calculateDeposit: jest.fn(),
  constructBaseTx: jest.fn(),
  constructTree: jest.fn(),
  constructTxToComplete: jest.fn(),
  signAndSubmitTx: jest.fn(),
}));

describe("MintDetailsDialog", () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should disable the mint button when the ADA tag is not available", async () => {
    checkAvailability.mockResolvedValue(false);

    const { getByText } = render(<MintDetailsDialog onMint={() => {}} />);

    const input = getByText(/enter ada tag/i);
    const button = getByText(/mint/i);

    fireEvent.change(input, { target: { value: "@adatag" } });

    await waitFor(() => expect(button).toBeDisabled());
  });

  it("should enable the mint button when the ADA tag is available", async () => {
    checkAvailability.mockResolvedValue(true);

    const { getByText } = render(<MintDetailsDialog onMint={() => {}} />);

    const input = getByText(/enter ada tag/i);
    const button = getByText(/mint/i);

    fireEvent.change(input, { target: { value: "@adatag" } });

    await waitFor(() => expect(button).not.toBeDisabled());
  });

  it("should show the confirmation dialog with deposit and timelock information when the mint button is clicked", async () => {
    checkAvailability.mockResolvedValue(true);
    calculateDeposit.mockResolvedValue({ deposit: 10, timelockActive: true, useAdaHandle: false });

    const { getByText, getByLabelText, queryByText } = render(<MintDetailsDialog onMint={() => {}} />);

    const input = getByLabelText(/enter ada tag/i);
    const button = getByText(/mint/i);

    fireEvent.change(input, { target: { value: "@adatag" } });
    fireEvent.click(button);

    await waitFor(() => getByText(/deposit: 10 ada/i));
    await waitFor(() => getByText(/a new timelock output will be created/i));

    expect(queryByText(/you already have an ada handle, no timelock output is required/i)).toBeNull();
  });

  it("should call onMint when the mint button in the confirmation dialog is clicked", async () => {
    const onMint = jest.fn();

    checkAvailability.mockResolvedValue(true);
    calculateDeposit.mockResolvedValue({ deposit: 10, timelockActive: true, useAdaHandle: false });
    signAndSubmitTx.mockResolvedValue({ success: true, txHash: "123" });

    const { getByText, getByLabelText } = render(<MintDetailsDialog onMint={onMint} />);

    const input = getByLabelText(/enter ada tag/i);
    const button = getByText(/mint/i);

    fireEvent.change(input, { target: { value: "@adatag" } });
    fireEvent.click(button);

    const confirmButton = getByText(/mint/i);
    fireEvent.click(confirmButton);

    await waitFor(() => expect(onMint).toHaveBeenCalled());
  });

  it("should show an error message when the sign and submit transaction fails", async () => {
    checkAvailability.mockResolvedValue(true);
    calculateDeposit.mockResolvedValue({ deposit: 10, timelockActive: true, useAdaHandle: false });
    signAndSubmitTx.mockResolvedValue({ success: false, error: "Test error" });

    const { getByText, getByLabelText, queryByText } = render(<MintDetailsDialog
        onMint={() => {}} />);

        const input = getByLabelText(/enter ada tag/i);
        const button = getByText(/mint/i);
        
        fireEvent.change(input, { target: { value: "@adatag" } });
        fireEvent.click(button);
        
        const confirmButton = getByText(/mint/i);
        fireEvent.click(confirmButton);
        
        await waitFor(() => getByText(/error: test error/i));
        
        expect(queryByText(/transaction submitted:/i)).toBeNull();
        });
        });
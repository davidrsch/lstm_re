describe("Tab-panel linkage - Selecting Features cards", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(1000);
  });

  it("Opening 'Training vectors' card selects 'Training vectors' guide tab", () => {
    cy.toggle_card("toggle_tv_card");
    cy.tab_should_be_active("Training vectors");
  });

  it("Opening 'Models options' card selects 'Models' guide tab", () => {
    cy.toggle_card("toggle_tv_card");
    cy.toggle_card("toggle_mo_card");
    cy.tab_should_be_active("Models");
  });

  it("Opening 'Training options' card selects 'Training' guide tab", () => {
    cy.toggle_card("toggle_to_card");
    cy.tab_should_be_active("Training");
  });

  it("Reopening 'Time series transformations' card selects 'Time series' guide tab", () => {
    cy.toggle_card("toggle_tv_card");
    cy.tab_should_be_active("Training vectors");
    cy.toggle_card("toggle_ts_card");
    cy.tab_should_be_active("Time series");
  });
});

describe("Tab-panel linkage - Upload Data cards", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
    cy.upload_csv_flow();
  });

  it("Opening variables card selects 'EDA' tab in Data Analysis panel", () => {
    cy.toggle_card("toggle_variables_card");
    cy.tab_should_be_active("EDA");
  });

  it("Reopening upload card selects 'Data' tab in Data Analysis panel", () => {
    cy.toggle_card("toggle_upload_card"); // collapse
    cy.toggle_card("toggle_upload_card"); // reopen
    cy.tab_should_be_active("Data");
  });
});

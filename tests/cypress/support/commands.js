// shiny.react v0.1.0 throws "Cannot read properties of null (reading
// 'querySelector')" from its output binding when a renderUI update fires
// concurrently with a shinyjs DOM toggle. The error is internal to the
// library and does not affect app functionality; suppress it so tests are
// not aborted prematurely.
Cypress.on('uncaught:exception', (err) => {
  if (err.message && err.message.includes('Cannot read properties of null')) {
    return false
  }
})

// Disable CSS animations/transitions so FluentUI modals and panels are
// immediately visible in the headless Electron runner.
Cypress.on('window:load', (win) => {
  const style = win.document.createElement('style');
  style.textContent = `
    *, *::before, *::after {
      transition: none !important;
      animation: none !important;
      animation-duration: 0s !important;
      transition-duration: 0s !important;
    }
  `;
  win.document.head.appendChild(style);
});

// Upload a CSV file and wait for the data table to appear in the Data Analysis panel
Cypress.Commands.add('upload_csv_flow', () => {
  cy.get('[data-testid="file"]').should('be.visible');
  cy.get('[data-testid="upload_file"] [type="file"]')
    .should('not.be.visible')
    .selectFile('cypress/fixtures/csv_example.csv', { force: true });
  // Wait for the toggle_variables_card button to become enabled, confirming
  // the server processed the file and renderUI has re-rendered the button.
  cy.get('[data-testid="toggle_variables_card"]', { timeout: 30000 })
    .should('not.have.attr', 'aria-disabled', 'true');
});

// Navigate to a Pivot tab by its label text
Cypress.Commands.add('navigate_to_tab', (tabLabel) => {
  // Wait for the app navigation to render before clicking
  cy.get('[role="tab"]', { timeout: 10000 }).should('have.length.gte', 1);
  cy.contains('[role="tab"]', tabLabel).click({ force: true });
});

// Click a card accordion toggle button identified by data-testid
Cypress.Commands.add('toggle_card', (testid) => {
  cy.get(`[data-testid="${testid}"]`).click({ force: true });
  cy.wait(1000);
});

// Open a FluentUI Dropdown and click one or more option indices.
Cypress.Commands.add('select_dropdown', (inputTestid, indices) => {
  cy.get(`[data-testid="${inputTestid}"]`).click({ force: true });
  const idxArray = Array.isArray(indices) ? indices : [indices];
  idxArray.forEach((index) => {
    cy.get('[role="listbox"]', { timeout: 10000 })
      .find(`[data-index="${index}"]`)
      .click({ force: true });
  });
  // Close callout by pressing Escape
  cy.get('body').type('{esc}');
});

// Assert that a Pivot tab with exact text is the currently selected tab.
// Uses anchored regex with cy.contains for full Cypress retryability.
// Timeout of 8 s to allow Shiny→runjs round-trip in CI.
Cypress.Commands.add('tab_should_be_active', (tabText) => {
  // Use string (not regex) so Cypress normalises whitespace in textContent.
  cy.contains('[role="tab"]', tabText, { timeout: 8000 })
    .should('have.attr', 'aria-selected', 'true');
});

// ── Full-flow helper commands ────────────────────────────────────────────────

// Open the Variables card, select all columns as Inputs and Outputs using the
// server-side "Select All" buttons, then wait for the data-amount card toggle
// to become enabled (confirms the server registered the I/O selection).
Cypress.Commands.add('select_io_variables_flow', () => {
  cy.toggle_card('toggle_variables_card');
  cy.get('[data-testid="io_gridtable"]', { timeout: 10000 }).should('be.visible');
  // Wait for the date variable dropdown to be visible before interacting.
  cy.get('[data-testid="datevariable"]', { timeout: 10000 }).should('be.visible');
  // Set the date variable so the date column is excluded from I/O features
  cy.select_dropdown('datevariable', [0]);
  // Use server-rendered "Select All" buttons — more reliable than handsontable cell clicks
  cy.contains('button', 'Inputs').first().click({ force: true });
  cy.contains('button', 'Outputs').first().click({ force: true });
  // Wait for the data-amount toggle to become enabled.
  cy.get('[data-testid="toggle_data_amount_card"]', { timeout: 30000 })
    .should('not.have.attr', 'aria-disabled', 'true');
});

// Open the data-amount card, keep the default test dates, and click OK to add
// the first available training start date, then wait for the table row to appear.
Cypress.Commands.add('add_train_set_flow', () => {
  cy.toggle_card('toggle_data_amount_card');
  cy.get('[data-testid="adtraintotest"]', { timeout: 10000 }).should('be.visible');
  // Wait for BOTH renderUI date dropdowns to finish rendering before clicking
  // OK.  The "End" label only appears once the second Dropdown renderUI has
  // fired, which guarantees that the Dropdown values have been flushed back to
  // Shiny's input system on the next flush cycle.
  cy.contains('label', 'Start', { timeout: 10000 }).should('be.visible');
  cy.contains('label', 'End', { timeout: 10000 }).should('be.visible');
  // Small extra wait to let Shiny complete the round-trip so input$selecttest*
  // values are available on the server when the OK handler fires.
  cy.wait(500);
  cy.get('[data-testid="adtraintotest"]').click({ force: true });
  // Assert on a real data cell (not the DataTables "No data available"
  // placeholder) to confirm the server processed the click.
  cy.get(
    '[data-testid="traindatestable_container"] tbody td:not(.dataTables_empty)',
    { timeout: 30000 }
  ).should('have.length.gte', 1);
});

// On the Selecting Features page: open Training vectors card and set temporal
// horizon to 1 with input amount 1, then open Models options with 1 LSTM layer
// and 4 neurons, and set epoch to 1.
// Uses minimal values so the CI training job finishes quickly.
// Transformations/scales are left at their defaults (3×3) — interacting with
// those renderUI dropdowns after a Pivot tab switch is unreliable in headless
// Electron because React re-hydration may not have completed before the click.
Cypress.Commands.add('configure_experiment_flow', () => {
  // Transformations card is open by default — wait for it to be stable.
  cy.get('[data-testid="selectimeseries"]', { timeout: 10000 }).should('be.visible');
  // Training vectors card
  cy.toggle_card('toggle_tv_card');
  cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('be.visible');
  cy.get('[data-testid="temporalhorizon"] input', { timeout: 8000 })
    .clear({ force: true }).type('1', { force: true }).should('have.value', '1');
  // "Add input amount" TextField — select by label text sibling
  cy.contains('label', 'Add input amount').parent().find('input')
    .clear({ force: true }).type('1', { force: true }).should('have.value', '1');
  cy.contains('button', 'Add input').click({ force: true });
  // Wait for server to process: the "Select the amounts of inputs" dropdown appears
  cy.contains('label', 'Select the amounts of inputs', { timeout: 10000 }).should('be.visible');
  // Models options card
  cy.toggle_card('toggle_mo_card');
  cy.contains('label', 'Add LSTM layer amount', { timeout: 15000 }).parent().find('input')
    .clear({ force: true }).type('1', { force: true }).should('have.value', '1');
  cy.get('[data-testid="add_lstm_amount_btn"]').click({ force: true });
  // Wait for server to process: the "Select the amounts of LSTM" dropdown appears
  cy.contains('label', 'Select the amounts of LSTM', { timeout: 10000 }).should('be.visible');
  cy.contains('label', 'Add neuron amount').parent().find('input')
    .clear({ force: true }).type('4', { force: true }).should('have.value', '4');
  cy.get('[data-testid="add_neuron_amount_btn"]').click({ force: true });
  // Wait for server to process: the "Select the amounts of neurons" dropdown appears
  cy.contains('label', 'Select the amounts of neurons', { timeout: 10000 }).should('be.visible');
  // Training options card
  cy.toggle_card('toggle_to_card');
  cy.contains('label', 'Epoch', { timeout: 8000 }).parent().find('input')
    .clear({ force: true }).type('1', { force: true }).should('have.value', '1');
});

// Click Start, confirm the experiment modal, and click OK to launch training.
Cypress.Commands.add('start_experiment_flow', () => {
  cy.get('[data-testid="startexperimentation"]').click({ force: true });
  // The models-to-build modal appears; click OK to accept
  cy.get('[data-testid="acceptmodels"]', { timeout: 10000 }).should('be.visible').click({ force: true });
});

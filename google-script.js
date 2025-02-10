// Code.gs
function doPost(e) {
  try {
    // Parse the incoming data
    const data = JSON.parse(e.postData.contents);
    
    // Get the active spreadsheet
    const ss = SpreadsheetApp.getActiveSpreadsheet();
    const sheet = ss.getSheetByName('Assessments') || ss.insertSheet('Assessments');
    
    // Set up headers if they don't exist
    if (sheet.getRange('A1').isBlank()) {
      sheet.getRange('A1:K1').setValues([[
        'Timestamp',
        'Student Number',
        'Name',
        'Surname',
        'Task',
        'Your Marks Total',
        'Max Marks Total',
        'Content Mark',
        'Code Formatting Mark',
        'Correct Answers',
        'Final Score'
      ]]);
      sheet.getRange('A1:K1').setFontWeight('bold');
    }
    
    // Prepare the row data
    const rowData = [
      new Date(),
      data.studentNumber,
      data.name,
      data.surname,
      data.task,
      data.totalYourMark,
      data.totalMaxMark,
      data.contentMark,
      data.codeFormattingMark,
      data.correctAnswers,
      data.finalScore
    ];
    
    // Add the data to the next empty row
    const lastRow = sheet.getLastRow();
    sheet.getRange(lastRow + 1, 1, 1, rowData.length).setValues([rowData]);
    
    // Return success response
    return ContentService.createTextOutput(JSON.stringify({
      status: 'success',
      message: 'Assessment submitted successfully'
    })).setMimeType(ContentService.MimeType.JSON);
    
  } catch (error) {
    // Return error response
    return ContentService.createTextOutput(JSON.stringify({
      status: 'error',
      message: error.toString()
    })).setMimeType(ContentService.MimeType.JSON);
  }
}

// Add this function to test the deployment
function doGet() {
  return ContentService.createTextOutput('The deployment is working correctly.');
}
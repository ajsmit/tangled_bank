// Import React and necessary hooks
import React, { useState, useEffect } from 'react';

// Replace this URL with your Google Apps Script deployment URL
const GOOGLE_SCRIPT_URL = 'YOUR_GOOGLE_APPS_SCRIPT_URL';

const AssessmentForm = () => {
  // Form state
  const [formData, setFormData] = useState({
    name: '',
    surname: '',
    studentNumber: '',
    task: '',
    yourMarks: Array(10).fill(''),
    maxMarks: Array(10).fill(''),
    contentMark: '',
    codeFormattingMark: '',
  });

  // Calculated values state
  const [calculatedValues, setCalculatedValues] = useState({
    totalYourMark: 0,
    totalMaxMark: 0,
    correctAnswers: 0,
    finalScore: 0
  });

  // Submission state
  const [submitStatus, setSubmitStatus] = useState({
    status: '',
    message: ''
  });
  const [isSubmitting, setIsSubmitting] = useState(false);

  // Handle changes to input fields
  const handleInputChange = (field, value) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
  };

  // Handle changes to mark inputs
  const handleMarkChange = (index, value, type) => {
    setFormData(prev => ({
      ...prev,
      [type === 'your' ? 'yourMarks' : 'maxMarks']: prev[type === 'your' ? 'yourMarks' : 'maxMarks'].map(
        (mark, i) => i === index ? value : mark
      )
    }));
  };

  // Handle form submission
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsSubmitting(true);
    setSubmitStatus({ status: '', message: '' });

    // Form validation
    if (!formData.name || !formData.surname || !formData.studentNumber || !formData.task) {
      setSubmitStatus({
        status: 'error',
        message: 'Please fill in all required fields'
      });
      setIsSubmitting(false);
      return;
    }

    try {
      const response = await fetch(GOOGLE_SCRIPT_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          ...formData,
          ...calculatedValues,
          timestamp: new Date().toISOString()
        })
      });

      const result = await response.json();

      if (result.status === 'success') {
        setSubmitStatus({
          status: 'success',
          message: 'Assessment submitted successfully!'
        });
      } else {
        throw new Error(result.message || 'Submission failed');
      }
    } catch (error) {
      setSubmitStatus({
        status: 'error',
        message: `Error submitting assessment: ${error.message}`
      });
    } finally {
      setIsSubmitting(false);
    }
  };

  // Calculate totals and final score whenever form data changes
  useEffect(() => {
    const totalYour = formData.yourMarks.reduce((sum, mark) => sum + (parseFloat(mark) || 0), 0);
    const totalMax = formData.maxMarks.reduce((sum, mark) => sum + (parseFloat(mark) || 0), 0);
    const correctAnswers = totalMax > 0 ? (totalYour / totalMax * 100) : 0;
    
    const contentMarkNum = parseFloat(formData.contentMark) || 0;
    const codeFormattingNum = parseFloat(formData.codeFormattingMark) || 0;
    
    const finalScore = (correctAnswers + contentMarkNum + codeFormattingNum) / 3;

    setCalculatedValues({
      totalYourMark: totalYour,
      totalMaxMark: totalMax,
      correctAnswers: correctAnswers.toFixed(2),
      finalScore: finalScore.toFixed(2)
    });
  }, [formData]);

  return (
    <div className="min-h-screen bg-gray-100 py-8">
      <form onSubmit={handleSubmit} className="max-w-4xl mx-auto">
        <div className="bg-white shadow-lg">
          {/* Status Alert */}
          {submitStatus.status && (
            <div className={`p-4 mb-4 ${
              submitStatus.status === 'success' ? 'bg-green-50 text-green-800' : 'bg-red-50 text-red-800'
            }`}>
              {submitStatus.message}
            </div>
          )}

          {/* Form Header */}
          <div className="p-6 border-b border-gray-200">
            <div className="text-center">
              <h1 className="text-2xl font-bold mb-2">BCB744 Assessment Form</h1>
              <p className="text-gray-600">Self-Assessment Template</p>
            </div>
          </div>

          {/* Main Form Content */}
          <div className="p-6 space-y-6">
            {/* Student Details */}
            <div className="bg-gray-50 p-4 rounded">
              <h2 className="text-lg font-semibold mb-4">Student Information</h2>
              <div className="grid grid-cols-3 gap-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Name</label>
                  <input
                    type="text"
                    className="w-full p-2 border rounded bg-white"
                    value={formData.name}
                    onChange={(e) => handleInputChange('name', e.target.value)}
                    required
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Surname</label>
                  <input
                    type="text"
                    className="w-full p-2 border rounded bg-white"
                    value={formData.surname}
                    onChange={(e) => handleInputChange('surname', e.target.value)}
                    required
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Student Number</label>
                  <input
                    type="text"
                    className="w-full p-2 border rounded bg-white"
                    value={formData.studentNumber}
                    onChange={(e) => handleInputChange('studentNumber', e.target.value)}
                    required
                  />
                </div>
              </div>
            </div>

            {/* Task Selection */}
            <div className="bg-gray-50 p-4 rounded">
              <h2 className="text-lg font-semibold mb-4">Task Selection</h2>
              <select
                className="w-48 p-2 border rounded bg-white"
                value={formData.task}
                onChange={(e) => handleInputChange('task', e.target.value)}
                required
              >
                <option value="">Select Task</option>
                {['A', 'B', 'C', 'D', 'E'].map(task => (
                  <option key={task} value={task}>Task {task}</option>
                ))}
              </select>
            </div>

            {/* Marks Table */}
            <div className="bg-gray-50 p-4 rounded">
              <h2 className="text-lg font-semibold mb-4">Assessment Marks</h2>
              <div className="overflow-x-auto">
                <table className="w-full border-collapse bg-white">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="border border-gray-300 p-2 text-left">Question</th>
                      <th className="border border-gray-300 p-2 text-left">Your Mark</th>
                      <th className="border border-gray-300 p-2 text-left">Max per question</th>
                    </tr>
                  </thead>
                  <tbody>
                    {formData.yourMarks.map((_, index) => (
                      <tr key={index}>
                        <td className="border border-gray-300 p-2">Question {index + 1}</td>
                        <td className="border border-gray-300 p-2">
                          <input
                            type="number"
                            className="w-full p-1 bg-white"
                            value={formData.yourMarks[index]}
                            onChange={(e) => handleMarkChange(index, e.target.value, 'your')}
                          />
                        </td>
                        <td className="border border-gray-300 p-2">
                          <input
                            type="number"
                            className="w-full p-1 bg-white"
                            value={formData.maxMarks[index]}
                            onChange={(e) => handleMarkChange(index, e.target.value, 'max')}
                          />
                        </td>
                      </tr>
                    ))}
                    <tr className="bg-gray-100">
                      <td className="border border-gray-300 p-2 font-bold">Total</td>
                      <td className="border border-gray-300 p-2 font-bold">{calculatedValues.totalYourMark}</td>
                      <td className="border border-gray-300 p-2 font-bold">{calculatedValues.totalMaxMark}</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>

            {/* Final Assessment Section */}
            <div className="bg-gray-50 p-4 rounded">
              <h2 className="text-lg font-semibold mb-4">Final Assessment</h2>
              <div className="space-y-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Content (%)</label>
                  <input
                    type="number"
                    className="w-48 p-2 border rounded bg-white"
                    value={formData.contentMark}
                    onChange={(e) => handleInputChange('contentMark', e.target.value)}
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">
                    Code Formatting & Presentation (%)
                  </label>
                  <input
                    type="number"
                    className="w-48 p-2 border rounded bg-white"
                    value={formData.codeFormattingMark}
                    onChange={(e) => handleInputChange('codeFormattingMark', e.target.value)}
                  />
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 mb-1">Correct Answers (%)</label>
                  <div className="w-48 p-2 border rounded bg-gray-100">
                    {calculatedValues.correctAnswers}%
                  </div>
                </div>
              </div>
            </div>

            {/* Final Score */}
            <div className="bg-gray-800 text-white p-6 rounded">
              <h3 className="text-xl font-bold text-center">
                FINAL SCORE: {calculatedValues.finalScore}%
              </h3>
            </div>
          </div>

          {/* Submit Button */}
          <div className="p-6 border-t border-gray-200">
            <div className="flex justify-between items-center">
              <button
                type="submit"
                disabled={isSubmitting}
                className={`px-6 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 
                  focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 
                  ${isSubmitting ? 'opacity-50 cursor-not-allowed' : ''}`}
              >
                {isSubmitting ? 'Submitting...' : 'Submit Assessment'}
              </button>
            </div>
          </div>

          {/* Form Footer */}
          <div className="p-6 border-t border-gray-200 text-center text-sm text-gray-600">
            BCB744 Assessment Form - Page 1 of 1
          </div>
        </div>
      </form>
    </div>
  );
};

export default AssessmentForm;
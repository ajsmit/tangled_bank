import { serve } from 'https://deno.fresh.dev/std@0.168.0/http/server.ts'
import { SmtpClient } from 'https://deno.land/x/smtp/mod.ts'

const SMTP_HOSTNAME = Deno.env.get('SMTP_HOSTNAME') || 'smtp.gmail.com'
const SMTP_PORT = parseInt(Deno.env.get('SMTP_PORT') || '587')
const SMTP_USERNAME = Deno.env.get('SMTP_USERNAME') || ''
const SMTP_PASSWORD = Deno.env.get('SMTP_PASSWORD') || ''

serve(async (req) => {
  try {
    const { studentEmail, name, surname, finalScore, component, task, submissionTime } = await req.json()

    const client = new SmtpClient()
    await client.connectTLS({
      hostname: SMTP_HOSTNAME,
      port: SMTP_PORT,
      username: SMTP_USERNAME,
      password: SMTP_PASSWORD,
    })

    const emailContent = `
    Dear ${name} ${surname},

    This email confirms your BCB744 assessment submission:

    Component: ${component}
    Task: ${task}
    Final Score: ${finalScore}%
    Submission Time: ${submissionTime}

    Please keep this email for your records.

    Best regards,
    BCB744 Team
    `

    await client.send({
      from: SMTP_USERNAME,
      to: studentEmail,
      subject: `BCB744 Assessment Submission - ${component} Task ${task}`,
      content: emailContent,
    })

    await client.close()

    return new Response(
      JSON.stringify({ message: 'Email sent successfully' }),
      {
        headers: { 'Content-Type': 'application/json' },
        status: 200,
      },
    )
  } catch (error) {
    return new Response(
      JSON.stringify({ error: error.message }),
      {
        headers: { 'Content-Type': 'application/json' },
        status: 500,
      },
    )
  }
})
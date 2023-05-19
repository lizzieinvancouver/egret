/* global use, db */

const { MongoClient } = require('mongodb');
const nodemailer = require('nodemailer');

const client = new MongoClient('mongodb+srv://UBC_User:IFCgP3ZKFUlhkTCL@remotetest.cls7o.mongodb.net/UBC_Dendrometer');

//Connect to Mongodb
async function connectToDatabase() {
  
    await client.connect();
    console.log('Connected to the MongoDB database');
  
}

connectToDatabase();

// Query the database and process the data
async function generateReport() {
  try {

    const db = client.db('UBC_Dendrometer');
    let report = 'Daily Report test\n\n';
    let count = 0;

    const collections = await db.listCollections().toArray();
    
    for (const collection of collections) {

      const lastElementCursor = await db.collection(collection.name).find().sort({_id:-1}).limit(1);
      const lastElement = await lastElementCursor.next();

      if(lastElement) {

        report += `${collection.name}\n`;
        count++;

        //Is Alginment Green?
        if (lastElement.AS5311 && lastElement.AS5311.Alignment === "Green") {
          report += `Alignment Green\n`;
        }
        else report += `Warning: Alignment Not Green!!!\n`;

        //Is Wifi 'ubcvisitor'?
        if (lastElement.WiFi && lastElement.WiFi.SSID === "ubcvisitor") {
          report += `Wifi 'ubcvisitor'\n`;
        }
        else report += `Warning: Wifi connection not 'ubcvisitor'!!!\n`;

        //Is Wifi working?
        if (lastElement.WiFi && lastElement.WiFi.RSSI === -100) {
          report += `Warning: RSSI == -100; Wifi not working!!!\n`;
        }
        else report += `Wifi working\n`;

        //Is battery voltage low ok?
        if (lastElement.Analog && lastElement.Analog.Vbat > 3.6) {
          report += `Battery V low ok\n`;
        }
        else report += `Warning: Battery voltage LOW!!!\n`;

        //Is battery voltage high ok?
        if (lastElement.Analog && lastElement.Analog.Vbat < 4.3) {
          report += `Battery V high ok\n`;
        }
        else report += `Warning: Battery voltage HIGH!!!\n`;

        //Check timestamp
        if (lastElement.Timestamp && lastElement.Timestamp.time_utc) {

          let timestamp = lastElement.Timestamp.time_utc;
          let hours = (new Date() - new Date(timestamp)) / 3600000;
          if(hours < 24) {
            report += `Data within 24h\n\n`;
          }
          else report += `Warning: Last data collection occurred more than 24 hours ago!!!\n\n`;

        } else {

          report += `Warning: Timestamp not available!!!\n\n`;

        }
        
      }

    }

    //How many dendrometers?
    report += `Number of dendrometers: ${count}\n`;

    //console.log(report);
    
    await sendEmail(report);
    return;


  } catch (error) {
    console.error('Error generating the report:', error);
  }
}

const report = generateReport();

//Send the email
async function sendEmail(report) {
  try {
    const transporter = nodemailer.createTransport({
      service: 'outlook',
      auth: {
        user: 'kimdajeong10@gmail.com',
        pass: 'SA,tfs2bBC',
      },
    });

    const mailOptions = {
      from: 'kimdajeong10@gmail.com',
      to: ['ece.dajeong.kim@gmail.com', 'frederik.baumgarten@ubc.ca'],
      subject: 'Daily Dendrometer Report',
      text: report,
    };

    await transporter.sendMail(mailOptions);
    console.log('Email sent successfully');
  } catch (error) {
    console.error('Error sending the email:', error);
  }
}
